## DBINTERSECT

### Description

A collection of geometry intersection processing tools.  Includes delusions of 
support for multiple database technologies with a single input test dataset and 
tests supporting each as needed.


### Dependencies

* Access to Oracle Spatial and/or PostGIS with privileges to create objects in a schema
* A database client and whatever it uses as standard command line input (SQL*Plus, psql, etc) 
* A terminal to execute shell scripts  


## Tests

I should be doing the gradle thing but for now we will call bespoke shell scripts that holler about FAILing when they fail.

### Tests: Oracle

Let's use PostgreSQL environmentals as Oracle inputs and be optimistic about the
future.  It is our repo my friends it is our rules the trick is to never be 
afraid.

```
$ export PGUSER=MSCHELL
$ export PGPASSWORD=OracleIsMyDatabae!
$ export PGDATABASE=DEVDB.DOITT.NYCNET
./dbintersect-test.sh oracle
```

### Tests: PostgreSQL

Some day friends


## Deploy

### Deploy: Oracle

```
$ export PGUSER=MSCHELL
$ export PGPASSWORD=OracleIsMyDatabae!
$ export PGDATABASE=DEVDB.DOITT.NYCNET
./dbintersect-deploy.sh oracle
```

### Deploy: PostgreSQL

TBD but probably like Oracle but with postgresql instead of oracle just a guess.