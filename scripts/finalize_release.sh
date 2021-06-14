#!/bin/sh

cd "$(dirname "$0")"/

sh ./update_version.sh
sh ./update_singleheader.sh
sh ./update_dollar_macros.sh
sh ./update_x_macros.sh
