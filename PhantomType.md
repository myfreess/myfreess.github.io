主要内容来源：https://www.haskellforall.com/2012/06/gadts.html

自Yoneda始，以GADTs终

数学家Alonzo Church (丘奇)在1933年创造了一个叫做λ演算的东东，它在后来被称为untyped λ calculus，是后世函数式编程理论的重要组成部分。此处有个写得非常好的简史↓

https://zhuanlan.zhihu.com/p/24648375

在 untyped LC中，有一种叫做Scott Encoding的技巧，用于表达递归数据类型。和接下来的内容有什么关系呢？关系不大，但Scott Encoding和函数式编程中堪称最著名设计模式的Continuation Passing Style有点关系。虽然我都浅尝辄止，不过看看总有些开阔眼界的好处。

https://crypto.stanford.edu/~blynn/compiler/scott.html

https://kseo.github.io/posts/2016-12-13-scott-encoding.html

还想再说几句抽象废话，有一个数学上的引理叫Yoneda Lemma(米田引理), 搬到haskell代码里面大概是这么回事

```haskell
forall a r . Functor f => (a -> r) -> f r
```

如果你有一个模式上符合上面这个签名的函数，那么这个函数其实可以约等于`f a`类型的一个值。

以上混话懒得看就当报菜名。

所谓的Phantom Type Parameter, 就是一些一眼看去在值构造子后面根本不出现的类型变量。比如我们定义个自然数看看

```haskell
data Nat size = Z | S (Nat size)
```

怪怪的! 再来个没有递归的类型定义让我们看得更清楚一点。

```haskell
newtype AbstractInt a = I Int
```

a的具体类型对于运行时根本没半毛钱影响，所以叫“幻影类型参数”。

其实haskell中还有个更怪的玩意：可以定义没有值的类型。

```haskell
data Zero
data Positive
data Negative
```

以上类型对于运行时无任何价值, 但是看看Data.Void模块......天啊，一堆abstract nonsense。

它的应用有点古怪，也许不是每个人都喜欢(我就不太喜欢，当然也可以说我是因为自己用不好才吃不到葡萄说葡萄酸.......)。

让我们从自然数定义着手吧，首要问题是应该把定义稍微改改。

```haskell
module Nat where

data Z
data S a

data Nat size = Z | S (Nat size)

two :: Nat (S (S Z))
two = S (S Z)
```

相信通过类型签名你已经发现了某些端倪，是的，类型的结构和值的结构非常类似!这能有什么应用呢？如果我们能通过某些方式对类型进行pattern matching和“运算”(Type Family 和 Functional Dependencies)，也许我们就可以在编译期提前预测常量表达式的性质：偶数还是奇数？是零吗？是质数吗？把相同的手段用到List上，就得到了函数式的Vector：类型中携带了长度信息。

但是直接使用Phantom type的约束能力是很弱的，比如这样也行。

```haskell
two :: Nat Z
two = S (S Z)
```

让我们重新审视一下值构造子，它们也是函数,也就是说它们有类型。

```haskell
Z :: Nat a
S :: Nat a -> Nat a
```

然而我们渴望的类型签名是这样:

```haskell
Z :: Nat Z
S :: Nat a -> Nat (S a)
```

这时候我们就需要Yoneda Lemma了，因为Nat是Functor，所以：

```haskell
Nat Z ≡ (Z -> a) -> Nat a
Nat a -> Nat (S a) ≡ Nat a -> (S a -> r) -> Nat r
```

那么根据这个引理重写一下Nat的定义

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Nat where

import Prelude hiding (succ)

data Z
data S a

data Nat size = Z (Z -> size) | forall a . S (S a -> size) (Nat a)

zero :: Nat Z
zero = Z id

succ :: forall a . Nat a -> Nat (S a)
succ n = S id n

two :: Nat (S (S Z))
two = succ (succ zero)
```

这个奇妙的trick在我第一次看到时真的有点震到。。。。。。但是文章开头那篇博客的目的不是真的让我们这么写代码，如果对那俩扩展感兴趣就看看，没兴趣算了。

感觉很好玩吧? 这下two的类型签名不可更改了，如果对这种利用编译器和额外类型参数对值的特征进行检查的东西感兴趣，还是建议搜索一下Dependent Type以及阅读The Little Typer获新生。haskell里面暂时还没有DT, 可以看看Idris(1/2)，它是一个新生的函数式语言，在haskell的基础上更加简洁优美。

最后，haskell的GADT扩展可以帮助我们把占位置用的id函数去掉。

```haskell
data Nat a where
    Z :: Nat Z
    S :: Nat a -> Nat (S a)
```

习惯了这种语法之后，也许可以去看看Agda?(我没有学会, waiting for you)
