set(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)

include_directories(${PROJECT_SOURCE_DIR}/src/hellolib)
add_executable(hello main.cpp)
target_link_libraries(hello hello_static)
