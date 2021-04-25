#!/bin/sh

cd "$(dirname "$0")"/../
# pip install quom
quom include/iter.hpp singleheader/temp.hpp --include_directory include --include_directory extern/extend/include

(echo "/*"; cat LICENSE; echo "*/"; echo; cat singleheader/temp.hpp) > singleheader/iter.hpp
rm singleheader/temp.hpp
