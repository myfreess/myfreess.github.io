# HasIt设计模式

> 原文：https://hackernoon.com/the-has-type-class-pattern-ca12adab70ae
>
> 注：这个Has并非Haskell的缩写。

## Q1 - Collecting all Images

想象一下你正在用haskell开发一个GUI下的游戏......不行，根本想象不出来!

这样吧, 假设平行世界的你在用haskell开发一个游戏, 现在正写到资源检查，需要确认各个场景的对应图像文件都在。

首先把场景(Scene)的类型设计好。

```haskell
data Scene = Scene 
  { backgroundImage   :: Text
  , characters        :: [Character]   
  , bewilderedTourist :: Maybe Character
  , objects           :: [Either Rock WoodenCrate]  
  }

data Character = Character
 { hat   :: Maybe DamageArray
 , head  :: DamageArray
 , torso :: DamageArray
 , legs  :: DamageArray
 , shoes :: Maybe DamageArray
 }

data DamageArray = DamageArray
  { noDamage        :: Text
  , someDamage      :: Text
  , excessiveDamage :: Text
  }

data Rock = Rock 
  { weight    :: Double
  , rockImage :: Text
  }

data WoodenCrate = WoodenCrate
  { strength         :: Double
  , woodenCrateImage :: DamageArray  
  }
```

要做的事情就是把对应图像文件名的Text收集起来，去重，挨个检查。最适合做这个容器的显然是`Data.Set`

```haskell
collectImages :: Scene -> Set Text
```

函数签名设计完毕!

但是我们可以看到，除了Scene本身所存储的图像文件名，还有很多图像文件名的信息是放在Scene所存储的子结构里的。甚至还带有Maybe，Either等上下文。

也许比较合适的方式是写多个函数，把每个子结构都考虑到。

```haskell
collectImages :: Scene -> Set Text
collectImages Scene {..} 
  =  singleton backgroundImage 
  <> mconcat (map collectCharacterImages characters)
  <> maybe mempty collectCharacterImages bewilderedTourist
  <> mconcat (map (either (singleton . collectRockImage) 
                          collectWoodenCrateImages)     
                  objects)
-- {..}是RecordWildCards扩展提供的语法糖
-- 食用方法: https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
-- 其他几个函数就不写了，多半对你很简单
```

烦啊! 计算机行业的一大老大难问题不就是命名吗! 是时候用typeclass来把这一堆收拾一下了。

```haskell
collectImages :: Scene -> Set Text
collectCharacterImages :: Character -> Set Text
collectDamageArrayImages :: DamageArray -> Set Text
collectRockImage :: Rock -> Text
collectWoodenCrateImages :: WoodenCrate -> Set Text
```

这么一看，其实它们的类型签名服从一个共同的模式: `a -> Set Text`，但是你可能要说有一个是`Rock -> Text`，为了一致性，我们包裹一下它的返回值，改成`Set Text`类型。这会带来性能上的损失，但是也有不少优点。

那么相应的typeclass设计如下:

```haskell
class HasImages a where
  images :: a -> Set Text
```

应该开始着手给Scene， Character，DamageArray几个写实例了吗? 先不要! 我们先定义几个派生规则，搞定Maybe，Either，List这几个「上下文」的处理。

```haskell
instance HasImages a => HasImages [a] where
  images xs = foldr (\x accum -> images x <> accum) mempty xs

instance HasImages a => HasImages (Maybe a) where
  images x = maybe [] images x

instance (HasImages a, HasImages b) => HasImages (Either a b) where
  images x = either images images x
```

把上下文处理单独拉出来写成实例是有好处的，如果在未来，Scene类型的定义需要修改，又加入了一些新的内容，并且仍然使用了这几个上下文的其中之一--这很有可能，那么就可以复用这几个实例的派生规则, 好处多多。

```haskell
instance HasImages Scene where
  images Scene {..} 
    =  singleton backgroundImage 
    <> images characters
    <> images bewilderedTourist
    <> images objects

instance HasImages Character where
  images Character {..} 
    =  images hat
    <> images head
    <> images torso
    <> images legs
    <> images shoes

instance HasImages DamageArray where
  images DamageArray {..} = fromList
    [ noDamage
    , someDamage
    , excessiveDamage
    ]

instance HasImages Rock where
  images Rock {..} = singleton rockImage

instance HasImages WoodenCrate where
  images WoodenCrate {..} = images woodenCrateImage
```

大功告成! 虽然增加了一些语法噪音和缩进问题，但是总的来说，代码更方便使用了(不过，有点不好理解了!)。

(可能有写过Scheme的朋友开始注意到了：怎么看着这么像个元求值器？虽然Scene类型并非一个AST，但是就是有这种感觉!)

如果有一组类型{a, b, c, d, e....},它们都可以提取出类型为μ的值，就可以采用HasIt模式了。下面的例子进一步提供了HasIt模式的一个更加泛化，通用的例子。


## Q2 - Convenient Argument Passing

假设要用haskell开发一个数据库应用，存点用户数据之类的东西。

```haskell
newtype Key a = Key UUID
data Entity a = Entity 
  { entityKey   :: Key a
  , entityValue :: a
  }
```

UUID 是 通用唯一识别码(Universally Unique Identifier)的缩写，总之就是用户在你这个平台的数据身份证，每个用户的UUID都是唯一的。
现在要写个函数针对某个用户查询ta的好友，是账户的好友不是朋友。

```haskell
getFriends :: Key User -> [Entity User]
```

现在你可能会有点疑惑，Key a中的a和值其实没半点瓜葛，为什么不这样定义呢？

```haskell
newtype Key = Key UUID
```

这实际上是一种被称为“PhantomType”的设计模式，在类型构造子的参数处刻意多出一或多个根本和值无关的参数，这些额外的类型参数可以自由地用于传递一些信息。当然了，一般来说，如果需要在类型中传递的信息或者信息处理的逻辑较为复杂，那就用GADTs和其他一些扩展。

回到正题，我们可能经常需要从Entity User去unpack出Key User使用，比如找出一个用户的好友的好友

```haskell
concatMap (getFriends . entityKey) (getFriends user)
```

那就不如弄个好用一点的API

```haskell
class HasKey a k | a -> k where
  key :: a -> Key k
```

这里使用的扩展为：`MultiParamTypeClasses`, `FunctionalDependencies`, 前者开启多参数的typeclass，后者则是那个 `| a -> k`的来源，意思是由参数a可以唯一确定一个对应的k，所以叫类似函数的依赖机制。由类型a可以确定唯一的类型k，听起来就很像函数嘛。

一个简单的使用教程在此: https://ocharles.org.uk/blog/posts/2014-12-14-functional-dependencies.html

一篇复杂的散文在此: https://aphyr.com/posts/342-typing-the-technical-interview

最后实现`HasKey`的实例和`getFriends`

```haskell
instance HasKey (Key a) a where
  key = id

instance HasKey (Entity a) a where
  key = entityKey

getFriends :: HasKey a User => a -> [Entity User]
```

非常简单，原作者说因为ta的一位同事(coworker应该是协作者，不过不知道是不是同事，可能是社区的朋友吧)很喜欢这个例子，所以仍然发出来了。

原作者还提到HasIt模式很适合结合haskell的MTL库使用，同时我个人推荐一下这篇标题叫做「Data types à la carte」的文章，你会看到Q1中的简单模式在结合上不动点理论，F-Algebra和FreeMonad之后可以做些什么。

```
http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
```
