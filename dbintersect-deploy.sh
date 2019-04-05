dbbrand=$1
if [ "$dbbrand" = "oracle" ]; then
    echo "compiling dbintersect"
    echo exit | sqlplus $PGUSER/$PGPASSWORD@$PGDATABASE @src/main/dbintersect-deploy-oracle.sql
elif [ "$dbbrand" = "postgresql" ]; then
    echo "some day friends"
else
    echo "unknown database type"
fi