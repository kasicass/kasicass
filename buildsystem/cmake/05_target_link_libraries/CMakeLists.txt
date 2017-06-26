cmake_minimum_required(VERSION 2.6)

project(SolutionName)

add_library(foo STATIC foo.c)

add_executable(bar bar.c)
target_link_libraries(bar foo)

