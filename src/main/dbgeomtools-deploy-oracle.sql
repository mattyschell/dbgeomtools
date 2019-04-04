CREATE OR REPLACE PACKAGE DBINTERSECT
AUTHID CURRENT_USER
AS

    TYPE numberarray IS TABLE OF NUMBER
    INDEX BY PLS_INTEGER;

    TYPE geomarray IS TABLE OF SDO_GEOMETRY
    INDEX BY PLS_INTEGER;

    PROCEDURE CLIPTABLEPOLYS (
        p_table         IN VARCHAR2
       ,p_clipper       IN VARCHAR2
       ,p_tablepkc      IN VARCHAR2 DEFAULT 'OBJECTID'
       ,p_clipperpkc    IN VARCHAR2 DEFAULT 'OBJECTID'
       ,p_tablegeom     IN VARCHAR2 DEFAULT 'SHAPE'
       ,p_clippergeom   IN VARCHAR2 DEFAULT 'SHAPE'
       ,p_tolerance     IN NUMBER DEFAULT .0005
    );

    FUNCTION GEOM_UNION (
        p_cursor        IN SYS_REFCURSOR
       ,p_tolerance     IN NUMBER DEFAULT .0005
       ,p_fetchlimit    IN NUMBER DEFAULT 5
    ) RETURN SDO_GEOMETRY;

    FUNCTION GEOM_INTERSECT (
        p_geom            IN SDO_GEOMETRY
       ,p_clipgeom        IN SDO_GEOMETRY
       ,p_tolerance       IN NUMBER DEFAULT .0005
    ) RETURN SDO_GEOMETRY DETERMINISTIC;

    FUNCTION POLY_INTERSECT (
        p_geom            IN SDO_GEOMETRY
       ,p_clipgeom        IN SDO_GEOMETRY
       ,p_tolerance       IN NUMBER
    ) RETURN SDO_GEOMETRY DETERMINISTIC;

    FUNCTION POLY_INTERSECT_ARR (
        p_geom              IN SDO_GEOMETRY
       ,p_clip_geom         IN SDO_GEOMETRY
       ,p_tolerance         IN NUMBER
       ,p_operation         IN VARCHAR2 DEFAULT 'INTERSECTION'
    ) RETURN DBINTERSECT.geomarray;

    FUNCTION SCRUB_POLY (
      p_geom                IN SDO_GEOMETRY
   ) RETURN DBINTERSECT.geomarray;

    FUNCTION LINE_INTERSECT (
       p_geom            IN SDO_GEOMETRY
      ,p_clipgeom        IN SDO_GEOMETRY
      ,p_tolerance       IN NUMBER
   ) RETURN SDO_GEOMETRY DETERMINISTIC;

    FUNCTION LINE_INTERSECT_ARR (
       p_geom            IN SDO_GEOMETRY
      ,p_clipgeom        IN SDO_GEOMETRY
      ,p_tolerance       IN NUMBER
   ) RETURN DBINTERSECT.geomarray DETERMINISTIC;

    FUNCTION SCRUB_LINE (
        p_geom    IN SDO_GEOMETRY
    ) RETURN DBINTERSECT.geomarray;

    FUNCTION SDO_ARRAY_TO_SDO (
        p_sdo_array   IN  DBINTERSECT.geomarray
    ) RETURN SDO_GEOMETRY;

    FUNCTION NUMBERARRAY_TO_VARRAY (
        p_input         IN DBINTERSECT.numberarray
    ) RETURN MDSYS.SDO_List_Type DETERMINISTIC;

END DBINTERSECT;
/


CREATE OR REPLACE PACKAGE BODY DBINTERSECT
AS

    PROCEDURE CLIPTABLEPOLYS (
        p_table         IN VARCHAR2
       ,p_clipper       IN VARCHAR2
       ,p_tablepkc      IN VARCHAR2 DEFAULT 'OBJECTID'
       ,p_clipperpkc    IN VARCHAR2 DEFAULT 'OBJECTID'
       ,p_tablegeom     IN VARCHAR2 DEFAULT 'SHAPE'
       ,p_clippergeom   IN VARCHAR2 DEFAULT 'SHAPE'
       ,p_tolerance     IN NUMBER DEFAULT .0005
    )
    AS

        -- The usual motivation:  
        -- Agency X provides geometries for their important territorial domains.
        -- Agency X does not care one bit about our important shorelines that
        --   all other datasets are aligned to. (usually planimetrics hydro)
        -- So rather than depict Agency X rowing about in the East River we 
        --   clip (intersect) Agency X territory by landmass. 
        -- Agency X territories = p_table
        -- Landmass Polygons = p_clipper
        -- 
        -- Complicating the motivation is the following
        --   Some (often most) records do not need to be clipped at all
        --   Some records need to be deleted entirely, but this is dangerous
        --   Intersecting 2 polygons can produce points and lines
        --
        -- Reminder that we only clip back and do not extend.
        -- Agency X polygons that wiggle back and forth near the shoreline
        --    will stop extending into the water, but where they wiggle toward 
        --    the interior of land they will not be aligned with the shoreline.
        -- This is a fun followup task to solve by you reader, the solution will
        --   bring you happiness and glory 

        psql                    VARCHAR2(4000);
        tablepassidz            DBINTERSECT.numberarray;
        tableintersectidz       DBINTERSECT.numberarray;
        clipintersectidz        DBINTERSECT.numberarray;
        table_id                PLS_INTEGER;
        next_table_id           PLS_INTEGER;
        interactingclipidz      DBINTERSECT.numberarray;
        my_cursor               SYS_REFCURSOR;
        clippedgeomarray        DBINTERSECT.geomarray;
        finalclipgeom           SDO_GEOMETRY;

    BEGIN

        -- we will stash these ids away.  they will not be intersected

        psql := 'select /*+ ORDERED */ '
             || 'a.' || p_tablepkc || ' '
             || 'from '
             || p_clipper || ' b, '
             || p_table || ' a '
             || 'where '
             || 'sdo_relate(a.' || p_tablegeom || ', b.' || p_clippergeom || ', :p1) = :p2 ';
        
        EXECUTE IMMEDIATE psql BULK COLLECT INTO tablepassidz USING 'mask=INSIDE+COVEREDBY+EQUAL'
                                                                   ,'TRUE';

        -- intersect each pair of interactions
        -- after completing the intersect of each table id against interacting
        --    clip geom, aggregate all surviving bits of table geoms
        --must perform this bit by interacting bit, even single pairs A<-->B
        --   because the intersection between two polys can result in a 
        --   collection with non-poly outputs
        
        psql := 'select /*+ ORDERED */ '
             || '   a.' || p_tablepkc || ' '
             || '  ,b.' || p_clipperpkc || ' '
             || 'from '
             || p_clipper || ' b, '
             || p_table || ' a '
             || '   where '
             || 'sdo_relate(a.' || p_tablegeom || ', b.' || p_clippergeom || ', :p1) = :p2 '
             || 'order by a.' || p_tablepkc || ', b.' || p_clipperpkc || ' ';

        EXECUTE IMMEDIATE psql BULK COLLECT INTO tableintersectidz
                                                ,clipintersectidz USING 'mask=CONTAINS+OVERLAPBDYDISJOINT+COVERS+OVERLAPBDYINTERSECT' 
                                                                       ,'TRUE';
         
        -- sample values for thinkin
        -- bring on the index errors computer teacher 
        -- 7	    1
        -- 9	    1
        -- 9	    2
        -- 10	    1

        FOR i IN 1 .. tableintersectidz.COUNT
        LOOP

            table_id        := tableintersectidz(i);

            IF i <> tableintersectidz.COUNT
            THEN
                
                next_table_id := tableintersectidz(i + 1);

            ELSE

                next_table_id := -1;

            END IF;

            IF table_id <> next_table_id
            THEN

                -- process this table id

                interactingclipidz(interactingclipidz.COUNT + 1) := clipintersectidz(i);

                psql := 'select '
                     || 'DBINTERSECT.geom_intersect(a.' || p_tablegeom || ', b.' || p_clippergeom || ', :p1) '
                     || 'from '
                     || '   ' || p_table || ' a '
                     || '  ,' || p_clipper || ' b '
                     || 'where '
                     || '    a.id = :p2 '
                     || 'and b.id in (select * from table(:p3)) ';

                OPEN my_cursor FOR psql USING p_tolerance
                                             ,table_id
                                             ,DBINTERSECT.numberarray_to_varray(interactingclipidz);

                finalclipgeom := dbintersect.GEOM_UNION(my_cursor
                                                       ,p_tolerance);

                CLOSE my_cursor;

                -- update statement this id

                psql := 'update '
                     || '   ' || p_table || ' a '
                     || 'set ' 
                     || '   a.' || p_tablegeom || ' = :p1 '
                     || 'where '
                     || '   a.' || p_tablepkc || ' = :p2 ';

                EXECUTE IMMEDIATE psql USING finalclipgeom
                                            ,table_id;

                COMMIT;

                interactingclipidz.DELETE;
                finalclipgeom := NULL;

            ELSE

                -- add this clip id to the array
                interactingclipidz(interactingclipidz.COUNT + 1) := clipintersectidz(i);

            END IF; 

        END LOOP;

        -- lastly any table id that hasn't been processed is either
        -- TOUCH or DISJOINT 
        -- (there is no explicit mask for disjoint, its != ANYINTERACT)
        -- dont need to test these, all ids not processed above should 
        -- be deleted 

        psql := 'delete from '
             || '   ' || p_table || ' a '
             || 'where '
             || '     a.' || p_tablepkc || ' not in (select * from table(:p1)) '
             || ' and a.' || p_tablepkc || ' not in (select * from table(:p2)) ';

        EXECUTE IMMEDIATE psql USING DBINTERSECT.numberarray_to_varray(tablepassidz)
                                    ,DBINTERSECT.numberarray_to_varray(tableintersectidz);

        COMMIT;        

    END CLIPTABLEPOLYS;


    FUNCTION GEOM_UNION (
        p_cursor        IN SYS_REFCURSOR
       ,p_tolerance     IN NUMBER DEFAULT .0005
       ,p_fetchlimit    IN NUMBER DEFAULT 5
    ) RETURN SDO_GEOMETRY
    AS

        -- pass in a cursor that returns a collection of sdo_geometry
        -- union the results and return one geom
        -- this is ineffectient, intent is to union geoms that are in memory
        -- (no spatial index)

        startgeomarray          DBINTERSECT.geomarray;
        finalgeom               SDO_GEOMETRY;

    BEGIN

        LOOP

            FETCH p_cursor BULK COLLECT INTO startgeomarray LIMIT p_fetchlimit;

            EXIT WHEN startgeomarray.COUNT = 0;

                FOR i IN 1 .. startgeomarray.COUNT
                LOOP

                    IF finalgeom IS NULL
                    THEN

                        finalgeom := startgeomarray(i);

                    ELSE

                        finalgeom := SDO_GEOM.sdo_union(finalgeom
                                                       ,startgeomarray(i)
                                                       ,p_tolerance);

                    END IF;

                END LOOP;

            END LOOP;

        RETURN finalgeom;

    END GEOM_UNION;


    FUNCTION GEOM_INTERSECT (
       p_geom            IN SDO_GEOMETRY
      ,p_clipgeom        IN SDO_GEOMETRY
      ,p_tolerance       IN NUMBER DEFAULT .0005
    ) RETURN SDO_GEOMETRY DETERMINISTIC
    AS

        -- p_geom is a single (multi) point, (multi) line, or (multi) poly type
        -- p_clipgeom is a single polygon which may be multiple rings 
        --    preferred call is p_clipgeom as a single outer ring polygon
        --    which has been pre-id'd to interact with our p_geom 
        -- SOP is to take some point, line, or poly features of interest
        --   and clip back to land or to some area of interest (like a borough) 

    BEGIN

        IF p_geom.SDO_GTYPE = 2001 
        OR p_geom.SDO_GTYPE = 2005
        THEN

            --points no prob
            RETURN SDO_GEOM.SDO_INTERSECTION(p_geom
                                            ,p_clipgeom
                                            ,p_tolerance);

        ELSIF p_geom.SDO_GTYPE = 2002 
        OR    p_geom.SDO_GTYPE = 2006
        THEN

            -- must scrub point intersections from output and reassemble
            RETURN DBINTERSECT.LINE_INTERSECT(p_geom
                                             ,p_clipgeom
                                             ,p_tolerance);

        ELSIF p_geom.SDO_GTYPE = 2003 
        OR    p_geom.SDO_GTYPE = 2007
        THEN

            -- must scrub line and point intersections from poly output
            RETURN DBINTERSECT.POLY_INTERSECT(p_geom
                                             ,p_clipgeom
                                             ,p_tolerance);

        ELSE

            RAISE_APPLICATION_ERROR(-20001,'Friend I have no idea what to do with SDO_GTYPE ' || p_geom.SDO_GTYPE || '!');
      
        END IF;

    END GEOM_INTERSECT;
    

    FUNCTION POLY_INTERSECT (
        p_geom            IN SDO_GEOMETRY
       ,p_clipgeom        IN SDO_GEOMETRY
       ,p_tolerance       IN NUMBER
    ) RETURN SDO_GEOMETRY DETERMINISTIC
    AS

        -- from GEOM_INTERSECT
        -- clipping polygons or multipolygons back to a polygon shape 

    BEGIN

        IF  p_geom.SDO_GTYPE != 2003 
        AND p_geom.SDO_GTYPE != 2007
        THEN
            RAISE_APPLICATION_ERROR(-20001,'POLY_INTERSECT: input shape is not polygon or multipolygon but ' || p_geom.SDO_GTYPE || '!');
        END IF;

        IF  p_clipgeom.SDO_GTYPE != 2003 
        AND p_clipgeom.SDO_GTYPE != 2007
        THEN
            RAISE_APPLICATION_ERROR(-20001,'intersection polygon geometry is not polygon or multipolygon but gtype ' || p_clipgeom.SDO_GTYPE || '!');
        END IF;

        RETURN SDO_ARRAY_TO_SDO(POLY_INTERSECT_ARR(p_geom
                                                  ,p_clipgeom
                                                  ,p_tolerance));

    END POLY_INTERSECT;


    FUNCTION POLY_INTERSECT_ARR (
        p_geom              IN SDO_GEOMETRY
       ,p_clip_geom         IN SDO_GEOMETRY
       ,p_tolerance         IN NUMBER
       ,p_operation         IN VARCHAR2 DEFAULT 'INTERSECTION'
    ) RETURN DBINTERSECT.geomarray
    AS

        -- call from POLY_INTERSECT
        -- scrub the return from sdo_intersection of worthless line and point
        --    intersection bits
        -- our return type (from scrub poly) is this packages bespoke array type
        -- caller to poly_intersect_arr must re-assemble into sdo_geometry  

        sdo_temp     SDO_GEOMETRY;
        output       DBINTERSECT.geomarray;

    BEGIN

        IF p_operation = 'INTERSECTION'
        THEN

            IF p_geom.SDO_GTYPE = 2003
            OR p_geom.SDO_GTYPE = 2007
            THEN

                RETURN SCRUB_POLY(SDO_GEOM.SDO_INTERSECTION(p_geom
                                                           ,p_clip_geom
                                                           ,p_tolerance));

            ELSE

                RAISE_APPLICATION_ERROR(-20001,'ERROR POLY_INTERSECT_ARR only works polygon or multipolygon inputs');
            
            END IF;

        ELSE

            -- possible difference clip goes here
            -- its a bad idea I think 
            -- but I may need to implement it to self-flagellate the idea away
            -- the idea is that SOP above is clip p_geom back to land
            --   (keep what intersects) 
            -- but sometimes what we have is not land but the opposite, water 
            --   that we must use as an eraser
            --   (discard what intersects)  

            RAISE_APPLICATION_ERROR(-20001,'Unknown operation ' || p_operation || '!');
      
        END IF;

    END POLY_INTERSECT_ARR;


    FUNCTION SCRUB_POLY (
        p_geom          IN SDO_GEOMETRY
    ) RETURN DBINTERSECT.geomarray
    AS

        subelement   SDO_GEOMETRY;
        output       DBINTERSECT.geomarray;
        pcounter     PLS_INTEGER;

    BEGIN

        -- garbage
        -- ideally the high level caller has not produced these types
        -- by never intersecting a polygon with a clip polygon that has 
        -- the relationship TOUCH  
        IF p_geom IS NULL
        OR p_geom.SDO_GTYPE = 2001
        OR p_geom.SDO_GTYPE = 2002
        OR p_geom.SDO_GTYPE = 2005
        OR p_geom.SDO_GTYPE = 2006
        THEN

            -- yeah I know, just looks better this way friends
            NULL;

        ELSIF p_geom.SDO_GTYPE = 2003
        THEN

            -- the usual, what we think of as the standard case
            output(1) := p_geom;
            
        ELSIF p_geom.SDO_GTYPE = 2004
        OR    p_geom.SDO_GTYPE = 2007
        THEN

            -- and finally the ugh results 
            pcounter := 1;
            
            FOR i IN 1 .. mdsys.SDO_UTIL.GETNUMELEM(p_geom)
            LOOP
            
                subelement := mdsys.SDO_UTIL.EXTRACT(p_geom, i);

                IF subelement.SDO_GTYPE = 2003
                THEN
               
                    output(pcounter) := subelement;
                    pcounter := pcounter + 1;
            
                END IF;
        
            END LOOP;

        ELSE
            
            RAISE_APPLICATION_ERROR(-20001,'incoming geometry cannot be processed with type ' || p_geom.sdo_gtype);
      
        END IF;

        RETURN output;

    END SCRUB_POLY;


    FUNCTION LINE_INTERSECT (
        p_geom          IN SDO_GEOMETRY
       ,p_clipgeom      IN SDO_GEOMETRY
       ,p_tolerance     IN NUMBER
    ) RETURN SDO_GEOMETRY DETERMINISTIC
    AS

        -- from GEOM_INTERSECT
        -- clipping lines or multilines back to polygons

    BEGIN

        IF  p_geom.SDO_GTYPE != 2002 
        AND p_geom.SDO_GTYPE != 2006
        THEN

            RAISE_APPLICATION_ERROR(-20001,'input line geometry is not line or multiline, but gtype ' || p_geom.SDO_GTYPE || '!');

        END IF;

        IF  p_clipgeom.SDO_GTYPE != 2003 
        AND p_clipgeom.SDO_GTYPE != 2007
        THEN

            RAISE_APPLICATION_ERROR(-20001,'intersection polygon geometry is not polygon or multipolygon but gtype ' || p_clipgeom.SDO_GTYPE || '!');

        END IF;

        RETURN SDO_ARRAY_TO_SDO(LINE_INTERSECT_ARR(p_geom
                                                  ,p_clipgeom
                                                  ,p_tolerance));

    END LINE_INTERSECT;


    FUNCTION LINE_INTERSECT_ARR (
        p_geom              IN SDO_GEOMETRY
       ,p_clipgeom          IN SDO_GEOMETRY
       ,p_tolerance         IN NUMBER
    ) RETURN DBINTERSECT.geomarray DETERMINISTIC
    AS

        -- call from LINE_INTERSECT
        -- scrub return from sdo_intersection of worthless point intersection bits
        -- our return type is package bespoke array type from scrub_line
        -- caller re-assembles into sdo_geometry 

    BEGIN

        RETURN SCRUB_LINE(SDO_GEOM.SDO_INTERSECTION(p_geom
                                                   ,p_clipgeom
                                                   ,p_tolerance));

    END LINE_INTERSECT_ARR;


    FUNCTION SCRUB_LINE (
        p_geom    IN SDO_GEOMETRY
    ) RETURN DBINTERSECT.geomarray
    AS

        subelement   SDO_GEOMETRY;
        output       DBINTERSECT.geomarray;
        pcounter     PLS_INTEGER;

   BEGIN

        IF p_geom IS NULL
        OR p_geom.SDO_GTYPE = 2001
        OR p_geom.SDO_GTYPE = 2003
        OR p_geom.SDO_GTYPE = 2005
        OR p_geom.SDO_GTYPE = 2007
        THEN

            --friends it is my code my rules
            NULL; 

        ELSIF p_geom.SDO_GTYPE = 2002
        THEN

            -- very legal and very cool
            output(1) := p_geom;

        ELSIF p_geom.SDO_GTYPE = 2004
        OR    p_geom.SDO_GTYPE = 2006
        THEN

            -- something else, scrub it
            pcounter := 1;

            FOR i IN 1 .. mdsys.SDO_UTIL.GETNUMELEM(p_geom)
            LOOP

                subelement := mdsys.SDO_UTIL.EXTRACT(p_geom,i);

                IF subelement.SDO_GTYPE = 2002
                THEN

                    output(pcounter) := subelement;
                    pcounter := pcounter + 1;

                END IF;

            END LOOP;

        ELSE

            RAISE_APPLICATION_ERROR(-20001,'incoming geometry cannot be processed with type ' || p_geom.sdo_gtype);
         
        END IF;

        RETURN output;

    END SCRUB_LINE;


    FUNCTION SDO_ARRAY_TO_SDO (
        p_sdo_array   IN  DBINTERSECT.geomarray
    ) RETURN SDO_GEOMETRY
    AS

        -- transform package convenience pl/sql geom array type
        -- back to sql compatible sdo_geometry
        -- caller should be sending in proper arrays with all elements
        --   of a single geom type

        output SDO_GEOMETRY;

    BEGIN

        FOR i IN 1 .. p_sdo_array.COUNT
        LOOP

            IF output IS NULL
            THEN

                output := p_sdo_array(i);
            
            ELSE
            
                output := mdsys.SDO_UTIL.APPEND(output
                                               ,p_sdo_array(i));
            
            END IF;

        END LOOP;

        RETURN output;

   END SDO_ARRAY_TO_SDO;


    FUNCTION NUMBERARRAY_TO_VARRAY (
        p_input         IN DBINTERSECT.numberarray
    ) RETURN MDSYS.SDO_List_Type DETERMINISTIC
    AS

        -- MDSYS.SDO_List_Type as VARRAY (2147483647) of Number

        -- varray types are inconvenient
        -- until we need to treat them as table types in SQL like
        -- SELECT t.* FROM 
        -- TABLE(DBINTERSECT.NUMBERARRAY_TO_VARRAY(numberarray)) t

        output      MDSYS.SDO_List_Type := MDSYS.SDO_List_Type();
        pcounter    PLS_INTEGER := 1;
        pkey        PLS_INTEGER;

    BEGIN

        output.EXTEND(p_input.COUNT);
        pkey := p_input.FIRST;
        
        LOOP

            EXIT WHEN NOT p_input.EXISTS(pkey);

            output(pcounter) := p_input(pkey);
            pcounter := pcounter + 1;

            pkey  := p_input.NEXT(pkey);

        END LOOP;

        RETURN output;

    END NUMBERARRAY_TO_VARRAY;

END DBINTERSECT;
/