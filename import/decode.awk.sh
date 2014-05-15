ls -1 ../cache/SourceForge | awk 'BEGIN {FS="."; print ""} { printf("%25s -> ", $1); print $1 | "base64 -d -"; close("base64 -d -"); print "" } END {print ""}'
