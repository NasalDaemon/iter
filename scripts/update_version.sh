#!/bin/sh

cd "$(dirname "$0")"/../

VERSION_FILE=include/iter/version.hpp

cat > $VERSION_FILE <<- EOM
#ifndef ITER_LIBRARY_VERSION
#  define ITER_LIBRARY_VERSION $(date +"%Y%m%d")
#endif
EOM
