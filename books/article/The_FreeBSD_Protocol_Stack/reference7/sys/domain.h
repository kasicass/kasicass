domain定义了某种communication的形式，比如：
AF_LOCAL(AF_UNIX)表示一个系统内进程间的通讯；
AF_ROUTE表示 user process 与 kernel 间共享 route table 的通讯；
AF_INET网络通讯

一个操作系统内，有多个 domain (AF_INET, AF_LOCAL, AF_ROUTE, ...)
一个 domain 对应多个 protocol，比如 AF_INET 对应 tcp, udp, ...

DOMAIN_SET(9)


---- data sturcture ----

domain 对应于 sys/domain.h 中的 struct domain{}
protocol 对应于 sys/protosw.h 中的 struct protosw{}

sys/domain.h
sys/protosw.h
kern/uipc_domain.c

netinet/in_proto.c
struct protosw inetsw[] (netinet/in_proto.c)


---- domain init ----
domaininit()       [kern/uipc_domain.c]
DOMAIN_SET *
domainfinalize()   [kern/uipc_domain.c]

为了配合 SYSINIT 的结构，将初始化的过程拆分为3步。


---- function ----

pffindproto, pffindtype
根据 PF_INET(family), SOCK_STREAM(type), IPPROTO_TCP(protocol) 这些信息，
返回对应的 struct protosw *

pfctlinput, pfctlinput2
调用所有 domain 下，所有 protocol 对应的 ctl_input 函数

