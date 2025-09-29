FromTo模式是haskell中实现数据转换常用模式,包括aeson, cassava, postgresql-simple在内的库都采用了这种模式。

From/To这个模式的关键在于尽可能不损失必要信息地在不同结构间进行转换。举个例子，Map类型表示的是键值对集合，至于要求键类型可比较，是为了让实现足够高效。所以转List时可以不理会顺序问题，以最直接的方式转换为关联列表。(实际实现为一种有序的二叉树，此处因我不会略过)

但是对于不同的需求，也可实现一些略微不同的变体函数。比如只取出值，或者只取出Key，又或者对转换出的List元素顺序有一定要求，以上Data.Map模块都有考虑到。

From/To模式的用途则多种多样，比如我要初始化一个Map，但是写二叉树很麻烦，写一堆insert也麻烦得紧，这时这样做就很方便:

```haskell
fromList [(5,"a"), (3,"b")]
```

想要进一步简化类似操作的话，GHC还有一些用于重载的扩展，主要针对List和String --> 也就是针对List，因为它有语法糖嘛。

https://ocharles.org.uk/blog/posts/2014-12-17-overloaded-strings.html

https://zhuanlan.zhihu.com/p/335175582

序列化/反序列化

real world的编程中除了数据结构转来转去，还有一些可能更加常用的操作: 序列化

包括Json，Sxml在内的众多结构化文本，都仰赖于序列化进行处理。haskell中文本处理的常见方式并非手工递归下降，也非各类Parser Generator，而是使用Monad进行轻量级的文本处理。

但是，在haskell著名的Json库Aseon中，为了符合haskell本身高度类型化的特征，没有使用Dynamic做类型擦除，而是选用Generic(不是泛型!) + Typeclass，设计了一个看来颇不可思议的API

https://www.zhihu.com/answer/122747359

http://dev.stephendiehl.com/hask/#json

其原理与ADT的基础结构有关，相关的文章已经很多了。烦请阅读下面这个教程的Polynomial functors和Polynomial functors for generic programming两节。

https://devtut.github.io/haskell/functor.html

如果觉得读完有些不过瘾，不妨再看看ADT的基础结构和Generic相关扩展如何使用。

https://segmentfault.com/a/1190000003943687

https://ocharles.org.uk/blog/posts/2014-12-16-derive-generic.html

如果你有点好奇“ADT这东西有没有什么更坚实的数学基础”，答案是有的! 但是haskell中的ADT比较弱，并不是所有的定义都能对应到数学定义上去。原因见此：https://cs.stackexchange.com/questions/55646/strict-positivity

对其数学来源的介绍则可参考「同构 - 编程中的数学」一书中第四章「范畴」的内容, 以及下面知乎问题的回答

https://www.zhihu.com/question/24460419

书是作者放在Github上的，随意下载。

https://github.com/liuxinyu95/unplugged

觉得自己可能性无限的话，还有Recursive types for free!这篇来自Philip Wadler的论文(×)草稿(✓)等着你。


以上，我又水了一篇我没学会的知识点汇总, 这也是日常了。正在看书准备大力丸中......


