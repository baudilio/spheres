# Top-level CMakeLists.txt for example
###
### Process this file with cmake to produce Makefile
###

set(lib_LIB_SRCS
    types.f90
    )

add_library(lib ${lib_LIB_SRCS})
