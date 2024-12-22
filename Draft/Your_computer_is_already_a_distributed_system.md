# 你的计算机其实是一个分布式系统

根据分布式系统领域老牌专家Lamport的说法，分布式系统是一种某台你不知道其存在的计算机挂了会导致你的电脑没法用的玩意

> A distributed system is one in which the failure of a computer you didn’t even know existed can render your own computer unusable
> 
> -- Leslie Lamport

在历史上，一般认为分布式系统有三个传统中心化系统不具备的特性：

+ 节点异质性(node heterogeneity): 不同的节点完全可以是不同类型的机器，拥有不同的指令集和架构
+ 动态性(dynamicity): 允许因为系统问题或者重新配置动态增删节点
+ 延迟(latency)：节点间消息传递的时间不是固定的，波动比较大

悲剧的是，现代计算机硬件完全满足这三个条件：

+ 中心化的计算机系统传统上假设所有共享内存的核心拥有相同的架构和性能取舍(performance tradeoffs), 但是拥有异质核心的计算机系统在现在的市场上越来越多了，最具有代表性的例子就是可编程的显卡/网卡，还有各种可拔插的FPGA应用。
+ 假如外设总线管理器挂了，所有外设都会下线。
+ NUMA的存在让访存延迟变得难以预测


于是乎，论文*Your computer is already a distributed system. Why isn’t your OS?*提出了硬件的基本特性改变给操作系统带来的新挑战：

