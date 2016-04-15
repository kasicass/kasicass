lo_clone_create(), 初始化 lo0 的 struct ifnet{}

lo_clone_destroy(), 释放 lo0 的 struct ifnet{}

这里的 clone 何意，不解。

loop_modevent(), kld module 的控制函数

looutput(), 做一些 route 的判断和 output packet 的统计，最后调用 if_simloop()

if_simloop(), 将output mbuf直接放入input queue中。其他很多地方也会用到 if_simloop()，比如单工的网卡(IFF_SIMPLEX)，其不能接收到来自于自己的broadcast，则需要if_simloop()模拟之。

loioctl(), 设置一些关于 loopback 的属性
