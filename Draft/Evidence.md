
`Int`, `Bool`, `[a]`, 每天编写haskell代码时总免不了要和这些类型打交道。连前端的函数式教程都免不了俗要提一嘴。类型检查挡住了很多傻逼兮兮的错误，救程序员于水深火热中。但是，haskell的Prelude中有很多所谓的「partial function」，它们非常简单，也很好理解，可惜并不能对所有符合类型签名的输入都给出一个合理的输出，这样一来，只好crash了。

显而易见空列表在其中居功甚伟，head, tail都没办法应付它。head只能无奈地在运行时输出一段“Prelude.head: empty list”, 坦白地说，就靠GHC我还没法知道到底错误出在哪一行。

其实这还算是一个比较好处理的问题，只要求传入的list非空就行，大不了我去用safeHead和monadic的错误处理。更加头大的问题是，假如对于参数的性质判断本身代价就很大呢？比如，需要一个已经被排序的list，但是每次只消耗一个元素。

性质检查可以通过函数完成

```haskell
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:s) = if x <= y then isSorted  (y:s) else False
```

这种返回`Bool`的函数一般称为谓词。

当然可以用RecursiveGo这个模式来避免每次递归都要检查一次，但是对于一个非常大的list来说，代价简直没法忍受。C程序员可能会说：我相信将来使用我的代码的人拥有充分的智慧，因此我无需多嘴多舌。毕竟自由的代价是永远保持警惕嘛! 

我不认为会有我以外的人使用我编写的haskell代码，考虑到我自己悲哀的记忆力与可怖的粗心大意，我又不能直接把要求写在文档/注释里了事。这就是Evidence模式起作用的时候了。

```haskell
module SortedList (sortT, unSL) where

import Data.List (sort)
import Data.Coerce

newtype SortedList a = SortedList [a]

sortT :: Ord a => [a] -> SortedList a
sortT = SortedList . sort

unSL :: SortedList a -> [a]
unSL = coerce
```

这个方案结合了NewType和SmartConstructor模式，虽然非常粗浅，但是通过SortedList这个新类型与合理的模块设计，我们可以在编译期拦截住不合理的输入并且避免了几乎所有的运行时开销!

我对Evidence模式的理解是，对于那些用谓词检查参数是否合适的场合，总是可以定义一个新类型存放合适的参数，上面这个例子很特殊，因为所有list都可以被处理为有序的list。在有些情况下，相应类型对应的值可以被谓词分为两类，比如在Int中区分质数与非质数。

```haskell
newtype Prime = Prime Int -- INVARIANT: must be prime

prime :: Int -> Maybe Prime
```

好办! 对于不合适的值通通返回Nothing就是!在要消耗值的函数内可以用case进行模式匹配，如果想更直接一点，就用PatternGuards这个扩展。介绍在此：https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/guide-to-ghc-extensions/pattern-and-guard-extensions#patternguards

Evidence模式的另一优点是增加代码可读性。

```haskell
-- Merge two sorted list into a new sorted list.
merge :: Ord a => SortedList a -> SortedList a -> SortedList a
```

但是等等，万一有人这样写代码怎么办？

```haskell
add :: (a -> Maybe Int) -> (a -> Maybe Int) -> a -> Maybe Int
add f g x =
    if isNothing (f x) || isNothing (g x)
    then Nothing
    else Just (fromJust (f x) + fromJust (g x))
```

原作者非常无奈地写下了这段话：

> Unfortunately, even if you follow the Evidence pattern in types, you still can misuse it in values. 

误用是没法避免的，优雅的语言也无法阻止失去理智的我瞎堆烂代码。

Evidence模式通过类型为一些值赋予额外的性质信息，可以在很多场景下代替谓词。思路类似的模式还有Phantom Type。也可以说，Phantom Type是Evidence模式的一种推广。

一些关于Evidence模式的讨论文章：

+ https://cs-syd.eu/posts/2016-07-24-overcoming-boolean-blindness-evidence.html

+ https://runtimeverification.com/blog/code-smell-boolean-blindness/

+ https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/

+ https://kataskeue.com/gdp.pdf



