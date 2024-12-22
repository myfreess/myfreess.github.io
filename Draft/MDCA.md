# 走向云应用现代开发方式

> Towards Modern Development of Cloud Applications

## 简介

微服务的挑战：

+ C1 : 损害性能。序列化数据和网络IO的开销逐渐变成了系统的性能瓶颈。
+ C2 : 损害正确性。一份调查(*Understanding and detecting software upgrade failures in distributed systems*, SOSP 2021)显示在8个被广泛使用的系统曾发生过的100个致命错误中，有三分之二是在升级时由不同版本的服务之间的交互导致的。
  > 看了一下这篇引用, 这8个系统分别是：Cassandra,the Hadoop Distributed File System (HDFS), Hadoop MapReduce, Hadoop YARN, HBase, Kafka, Mesos, ZooKeeper.
+ C3 : 很难管理。N个二进制文件伴随着N个发布流程。
+ C4 : API僵化
+ C5 : 拖慢应用开发

为什么微服务没有解决这些问题：


+ 微服务假设用户手动将应用分割为多个二进制
+ 微服务假设这些二进制会被分别部署到生产环境

这篇论文的解决方案：

既然程序员手动搞容易搞出问题，不如弄个运行时帮程序员管应用切分部署更新这些问题，程序员只要写承载业务逻辑的**组件(Component)**即可，每个组件对应一个微服务，而单个微服务的API使用类型系统中和接口类似的概念表达。

> 此处请参考Erlang语言的Actor

组件中只包含业务逻辑，但是最后分发到机器上跑还是要有负责和实际运行环境交互的程序，这些程序被叫做**proclet**，它可以控制组件的启动运行还有在组件出错时重启。

> 此处请参考Erlang/OTP的supervisor