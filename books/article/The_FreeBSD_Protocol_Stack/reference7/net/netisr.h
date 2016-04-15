netisr (network interrupt service routine)

各个 domain(family) 协议的 input handler dispatch
网络数据到达，mbuf可以直接处理，也可以丢到ifqueue中，等待interrupt时再处理

IPv4     IPv6      Route     ...
 |         |         |        |
 ------------------------------
           ^
           |
      +----------+
      |  netisr  |
      +----------+
