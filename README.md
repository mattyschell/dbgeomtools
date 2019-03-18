## DBGEOMTOOLS

### Description

A collection of database geometry processing tools.  Delusions of support for
multiple database technologies with a single input test dataset and 
tests supporting each database as needed.


### Dependencies

* python (3.0)
* Oracle Spatial and/or PostGIS with privileges to create objects in a schema


### Test: Oracle

Thinking about doing the Gradle thing but let's not get ahead of ourselves.

Let's use PostgreSQL environmentals as Oracle inputs and be optimistic about the
future.  It is our repo my friends it is our rules the trick is to never be 
afraid.

```
$ export PGUSER=MSCHELL
$ export PGPASSWORD=OracleIsMyDatabae!
$ export PGDATABASE=DEVDB.DOITT.NYCNET
python test_dbgeomtools.py oracle
```

example: 

`python test_geomtools.py`





