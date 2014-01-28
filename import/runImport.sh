#!/bin/sh


echo ""
echo "Starting import process..."
echo "=========================="

echo "Importing FLOSSmole data..."
#./getFLOSSmoleData.R

echo "Importing SourceForge data..."
#./getSourceForgeData.R

echo "Importing AngelList data..."
#./getAngelListData.R

echo "Importing CrunchBase data..."
export CRUNCHBASE_API_KEY=uuxr6qxxm3be8zwbpt5kuvs2
#./getCrunchBaseDataAPI.R

echo "=========================="
echo "Data import finished successfully."
echo ""