#!/bin/sh

cd "$(dirname "$0")"/../

DEFINE_FILE=include/iter/dollar_macros/define.hpp

cat > $DEFINE_FILE <<- EOM
#ifndef ITER_DOLLAR_MACROS_DEFINE_HPP
#define ITER_DOLLAR_MACROS_DEFINE_HPP

#include "extend/dollar_macros/define.hpp"

EOM

grep -oE '(ITER_DECLARE\([^\)]+\)$)|(ITER_ALIAS\([^,]+,.*\)$)' singleheader/iter.hpp \
    | sed -E 's|ITER_DECLARE\((\w+)\)|#define $\1 $(::iter::\1)|g' \
    | sed -E 's|ITER_ALIAS\(([^,]+),.*\)|#define $\1 $(::iter::\1)|g' \
    >> $DEFINE_FILE
grep -oE 'struct \w+ : xtd::tagged_bindable' singleheader/iter.hpp \
    | sed -E 's|struct (\w+) : xtd::tagged_bindable|#define $\1(...) $(::iter::\1<__VA_ARGS__>)|g' \
    >> $DEFINE_FILE

cat >> $DEFINE_FILE <<- EOM

#endif /* ITER_DOLLAR_MACROS_DEFINE_HPP */
EOM

UNDEF_FILE=include/iter/dollar_macros/undef.hpp

cat > $UNDEF_FILE <<- EOM
#ifdef ITER_DOLLAR_MACROS_DEFINE_HPP
#undef ITER_DOLLAR_MACROS_DEFINE_HPP

#include "extend/dollar_macros/undef.hpp"

EOM

grep -oE '(ITER_DECLARE\([^\)]+\)$)|(ITER_ALIAS\([^,]+,.*\)$)' singleheader/iter.hpp \
    | sed -E 's|ITER_DECLARE\((\w+)\)|#undef $\1|g' \
    | sed -E 's|ITER_ALIAS\(([^,]+),.*\)|#undef $\1|g' \
    >> $UNDEF_FILE
grep -oE 'struct \w+ : xtd::tagged_bindable' singleheader/iter.hpp \
    | sed -E 's|struct (\w+) : xtd::tagged_bindable|#undef $\1|g' \
    >> $UNDEF_FILE

cat >> $UNDEF_FILE <<- EOM

#endif /* ITER_DOLLAR_MACROS_DEFINE_HPP */
EOM
