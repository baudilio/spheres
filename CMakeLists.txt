# Top-level CMakeLists.txt for example
###
### Process this file with cmake to produce Makefile
###

cmake_minimum_required(VERSION 3.20)

project(example Fortran)

set(PACKAGE spheres)

# List of subdirectories to consider
add_subdirectory(prog)
add_subdirectory(lib)
