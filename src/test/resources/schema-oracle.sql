BEGIN
    EXECUTE IMMEDIATE 'DROP TABLE TABLE_POLYGONS';
EXCEPTION  WHEN OTHERS THEN 
    IF SQLCODE = -942 
        THEN NULL;
    ELSE 
        RAISE;
    END IF;
END;
CREATE TABLE table_polygons ( 
    id          NUMBER
   ,mask        VARCHAR2(32)
   ,testgroup   NUMBER
   ,shape       SDO_GEOMETRY
   ,CONSTRAINT table_polygonspkc PRIMARY KEY (id)
   );
DELETE FROM user_sdo_geom_metadata where table_name = 'TABLE_POLYGONS';
INSERT INTO user_sdo_geom_metadata a ( 
     table_name
    ,column_name
    ,srid
    ,diminfo
) VALUES (
     'TABLE_POLYGONS'
    ,'SHAPE'
    ,2263
    ,SDO_DIM_ARRAY ( MDSYS.SDO_DIM_ELEMENT ('X', 900000, 1090000, .0005)
                    ,MDSYS.SDO_DIM_ELEMENT ('Y', 110000, 295000, .0005))
);
CREATE INDEX table_polygonsshaidx ON table_polygons (SHAPE) INDEXTYPE IS MDSYS.SPATIAL_INDEX;
--
--
BEGIN
    EXECUTE IMMEDIATE 'DROP TABLE QUERY_POLYGONS';
EXCEPTION  WHEN OTHERS THEN 
    IF SQLCODE = -942 
        THEN NULL;
    ELSE 
        RAISE;
    END IF;
END;
CREATE TABLE query_polygons (
    id          NUMBER
   ,testgroup   NUMBER
   ,shape       SDO_GEOMETRY
   ,CONSTRAINT query_polygonspkc PRIMARY KEY (id));
DELETE FROM user_sdo_geom_metadata where table_name = 'QUERY_POLYGONS';
INSERT INTO user_sdo_geom_metadata a ( 
     table_name
    ,column_name
    ,srid
    ,diminfo
) VALUES (
     'QUERY_POLYGONS'
    ,'SHAPE'
    ,2263
    ,SDO_DIM_ARRAY ( MDSYS.SDO_DIM_ELEMENT ('X', 900000, 1090000, .0005)
                    ,MDSYS.SDO_DIM_ELEMENT ('Y', 110000, 295000, .0005))
);
CREATE INDEX query_polygonsshaidx ON query_polygons (SHAPE) INDEXTYPE IS MDSYS.SPATIAL_INDEX;