#!/bin/bash

echo ""
for f in ../cache/SourceForge/*; do
  base=$(basename $f .RData)
  echo "$base  <=>  $(base64 -d <<<$base)"
done
echo ""
