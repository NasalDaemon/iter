#!/bin/sh

cd "$(dirname "$0")"/../

X_MACROS_FILE=include/iter/x_macros/iter_functions.hpp

grep -oE '(ITER_DECLARE\([^\)]+\)$)|(ITER_ALIAS\([^,]+,.*\)$)' singleheader/iter.hpp \
    | grep -v 'to_iter' \
    | sed -E 's|ITER_DECLARE\((\w+)\)|// Invoke iter::\1 on this iter\nITER_X(\1)|g' \
    | sed -E 's|ITER_ALIAS\(([^,]+),\s*(.*)\)|// Invoke iter::\1 (aka iter::\2) on this iter\nITER_X(\1)|g' \
    > $X_MACROS_FILE
