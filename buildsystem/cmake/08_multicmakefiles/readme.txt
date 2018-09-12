ADD_SUBDIRECTORY(source_dir [binary_dir] [EXCLUDE_FROM_ALL])
这个指令用于向当前工程添加存放源文件的子目录，并可以指定中间二进制和目标二进制存
放的位置。EXCLUDE_FROM_ALL参数的含义是将这个目录从编译过程中排除，比如，工程
的example，可能就需要工程构建完成后，再进入example目录单独进行构建(当然，你
也可以通过定义依赖来解决此类问题)。

上面的例子定义了将src子目录加入工程，并指定编译输出(包含编译中间结果)路径为
bin目录。如果不进行bin目录的指定，那么编译结果(包括中间结果)都将存放在
build/src目录(这个目录跟原有的src目录对应)，指定bin目录后，相当于在编译时
将src重命名为bin，所有的中间结果和目标二进制都将存放在bin目录。

换个地方保存目标二进制
不论是SUBDIRS还是ADD_SUBDIRECTORY指令(不论是否指定编译输出目录)，我们都可
以通过SET指令重新定义EXECUTABLE_OUTPUT_PATH和LIBRARY_OUTPUT_PATH变量
来指定最终的目标二进制的位置(指最终生成的hello或者最终的共享库，不包含编译生成
的中间文件)
SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)

