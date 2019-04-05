dbbrand=$1
./dbintersect-deploy.sh "$dbbrand"
if [ "$dbbrand" = "oracle" ]; then
    echo "testing dbintersect"
    echo exit | sqlplus $PGUSER/$PGPASSWORD@$PGDATABASE @src/test/test-oracle.sql
elif [ "$dbbrand" = "postgresql" ]; then
    echo "some day friends"
else
    echo "unknown database type"
fi