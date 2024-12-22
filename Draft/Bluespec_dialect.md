# bluespec的基本想法 - 基于规则的硬件设计

> 本文内容来源于The Essence of Bluespec - A Core Language for Rule-Based Hardware Design

bluespec的cost model不是很清晰，性能的细节由用户标注(user hint)和一些不透明的关于并发冲突的静态分析结果混合决定，所以弄一个方言出来，它的操作语义允许直接控制调度，有运行时分析可以避免并发异常(anomalies), 然后再展示如何用静态分析去除运行时的分析。

## 