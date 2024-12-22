# 弹性LL解析器

> https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html

## 简介

这篇教程要做的事情是解释一种适合语言服务器的解析技术，它天生就能处理不完整的代码片段。要完整展示这个问题和解决方案需要一些不那么简单的例子，此外作者还想分享一些并不和弹性直接相关的小技巧。


这篇教程是描述性的而非指示性的 - 它只提供一些可行的选择，如何根据实际情况选择设计是读者的事情。如果你正在思考如何构建一个产品级的语言服务器，把这篇教程当作点子图书馆，而非蓝图。

如果你想快速构建点什么能用的，请看tree-sitter。

如果你正在从头构建一个IDE级别的解析器，这些技术也许可以直接使用。

## 为什么需要弹性？

让我们看一个能解释弹性解析技术意图的例子

```rust
fn fib_rec(f1: u32,

fn fib(n: u32) -> u32 {
  fib_rec(1, 1, n)
}
```

Here, a user is in the process of defining the fib_rec helper function. For a language server, it’s important that the incompleteness doesn’t get in the way. In particular:

    The following function, fib, should be parsed without any errors such that syntax and semantic highlighting is not disturbed, and all calls to fib elsewhere typecheck correctly.

    The fib_rec function itself should be recognized as a partially complete function, so that various language server assists can help complete it correctly.

    In particular, a smart language server can actually infer the expected type of fib_rec from a call we already have, and suggest completing the whole prototype. rust-analyzer doesn’t do that today, but one day it should.

Generalizing this example, what we want from our parser is to recognize as much of the syntactic structure as feasible. It should be able to localize errors — a mistake in a function generally should not interfere with parsing unrelated functions. As the code is read and written left-to-right, the parser should also recognize valid partial prefixes of various syntactic constructs.

Academic literature suggests another lens to use when looking at this problem: error recovery. Rather than just recognizing incomplete constructs, the parser can attempt to guess a minimal edit which completes the construct and gets rid of the syntax error. From this angle, the above example would look rather like fn fib_rec(f1: u32, /* ) {} */ , where the stuff in a comment is automatically inserted by the parser.

Resilience is a more fruitful framing to use for a language server — incomplete code is the ground truth, and only the user knows how to correctly complete it. An language server can only offer guesses and suggestions, and they are more precise if they employ post-parsing semantic information.

Error recovery might work better when emitting understandable syntax errors, but, in a language server, the importance of clear error messages for syntax errors is relatively lower, as highlighting such errors right in the editor synchronously with typing usually provides tighter, more useful tacit feedback.

## 弹性错误处理方法

经典的解析器错误处理方法是错误产生式(error produnction)和同步词法单元(synchronization tokens).

这不适合弹性方法，考虑每一种可能的错误并加进语法太麻烦了，最好是能还原出尽可能多的合法语法树，然后跳过不合法的部分。

Tree-sitter做了一些更有趣的事情，它是一个GLR解析器，那意味着它会非确定性地尝试多次可行的LR解析，并选择最好的结果。这让tree-sitter可以挖掘出很多个完整的语法树小片段，不过在组合成更大片段的时候可能会有问题。

Tree-sitter does something more interesting. It is a GLR parser, meaning that it non-deterministically tries many possible LR (bottom-up) parses, and looks for the best one. This allows Tree-sitter to recognize many complete valid small fragments of a tree, but it might have trouble assembling them into incomplete larger fragments. In our example fn fib_rec(f1: u32, , Tree-sitter correctly recognizes f1: u32 as a formal parameter, but doesn’t recognize fib_rec as a function.

Top-down (LL) parsing paradigm makes it harder to recognize valid small fragments, but naturally allows for incomplete large nodes. Because code is written top-down and left-to-right, LL seems to have an advantage for typical patterns of incomplete code. Moreover, there isn’t really anything special you need to do to make LL parsing resilient. You sort of… just not crash on the first error, and everything else more or less just works.

Details are fiddly though, so, in the rest of the post, we will write a complete implementation of a hand-written recursive descent + Pratt resilient parser.

## 引入L语言

## 设计语法树

## 解析器

## 文法

## 基本的弹性

## 提升弹性

## 