# Top-level CMakeLists.txt for example
###
### Process this file with cmake to produce Makefile
###

include_directories(${CMAKE_BINARY_DIR}/lib)

add_executable(example.x main.f90)

target_link_libraries(example.x lib)

install(PROGRAMS example.x
  TYPE BIN
  )
