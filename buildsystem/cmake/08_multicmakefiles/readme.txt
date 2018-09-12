ADD_SUBDIRECTORY(source_dir [binary_dir] [EXCLUDE_FROM_ALL])
这个指令用于向当前工程添加存放源文件的子目录，并可以指定中间二进制和目标二进制存
放的位置。EXCLUDE_FROM_ALL参数的含义是将这个目录从编译过程中排除，比如，工程
的example，可能就需要工程构建完成后，再进入example目录单独进行构建(当然，你
也可以通过定义依赖来解决此类问题)。
