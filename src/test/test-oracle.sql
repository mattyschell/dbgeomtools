@src/test/resources/schema-oracle.sql
@src/test/resources/data-oracle.sql
@src/main/dbgeomtools-deploy-oracle.sql
call dbintersect.cliptablepolys('TABLE_POLYGONS','QUERY_POLYGONS','ID','ID');
select 'FAIL, this id should have been clipped: ' || to_char(id) from table_polygons
minus
select 'FAIL, this id should have been clipped: ' || to_char(id) from table_polygons_clipped;
select 'FAIL, this id was not clipped as expected ' || a.id from 
   table_polygons a
  ,table_polygons_clipped b
where 
    a.id = b.id
and sdo_geom.relate(a.shape, 'mask=DETERMINE', b.shape, .0005) <> 'EQUAL'; 
EXIT