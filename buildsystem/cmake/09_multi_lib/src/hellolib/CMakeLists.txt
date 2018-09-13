set(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)

set(LIBHELLO_SRC hello.cpp)

add_library(hello_dll SHARED ${LIBHELLO_SRC})
set_target_properties(hello_dll PROPERTIES OUTPUT_NAME "hello")

add_library(hello_static STATIC ${LIBHELLO_SRC})
set_target_properties(hello_static PROPERTIES OUTPUT_NAME "hello")
