#!/bin/sh

cd "$(dirname "$0")"/

sh ./update_version.sh
sh ./update_singleheader.sh
sh ./update_dollar_macros.sh
sh ./update_x_macros.sh
# Circular dependency: generate singleheader again
# update_singleheader -> update_x_macros -> update_singleheader
sh ./update_singleheader.sh
