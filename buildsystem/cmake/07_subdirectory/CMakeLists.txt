cmake_minimum_required(VERSION 2.6)
project(directory_test)

# Bring the headers, such as student.h into the project
include_directories(include)

# Can manually add the sources using the set command as follows:
#set(SOURCES src/main.cpp src/student.cpp)

# However, the file(GLOB...) allows for wildcard additions:
file(GLOB SOURCES "src/*.cpp")

add_executable(testStudent ${SOURCES})

