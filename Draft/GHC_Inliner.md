# 1999年以前的GHC Inliner

> Secrets of the Glasgow Haskell Compiler inliner读书笔记

## 1 Introduction

在函数式语言中，内联操作合并了其他数种传统上分离开来的优化。

这篇论文的意图在于描写多个**作者一开始搞错了的方面**。许多inline相关论文的着眼点是一个在许多地方调用的函数到底要不要inline，这个当然很重要，但另外一些较少人关注的问题也很有趣：

+ 任何编译器在做inline时都要处理名称捕获(*name capture*), GHC一开始使用的暴力法使用起来很不舒服。

+ 对于递归定义，GHC一开始是不inline的。后来他们发现有些情况下这样做性能太差，在经过数次失败后他们弄出了一个简单的算法干这事。

+ GHC会在三个不同阶段做inline。

+ 可以根据动态环境的信息做inline

## 2 Preliminaries

```haskell
type Program = [Bind]
data Bind = NonRec Var Expr
  | Rec [(Var, Expr)]
data Expr = Var Var
  | App Expr Expr
  | Lam Var Expr
  | Let Bind Expr
  | Const Const [Expr]
  | Case Expr Var [Alt]
  | Note Note Expr
type Alt -- Case alternative
  = (Const, [Var], Expr)
data Const -- Constant
  = Literal Literal
  | DataCon DataCon
  | PrimOp PrimOp
  | DEFAULT
```

### 2.1 什么是内联

+ *Inlining itself*
  
  ```
    let { f = \x -> x * 3 } in f (a + b) - c
  ====> [inline f]
    let { f = \x -> x * 3 } in (\x -> x * 3) (a + b) - c
  ```

+ *Dead code elimination*
  
  ```
    let { f = \x -> x * 3 } in (\x -> x * 3) (a + b) - c
  ====> [dead f]
    (\x -> x * 3) (a + b) - c
  ```

+ *beta-reduction*
  
  ```
    (\x -> x * 3) (a + b) - c
  ====> [beta]
    (let { x = a + b } in x * 3) - c
  ```

在haskell这种纯函数式语言中

### 2.2 影响内联的因素

### 2.3 工作重复

## 3 名称捕获

### 3.1 全局重命名

### 3.2 The Repier

### 3.3 选择新名字

### 3.4 看看效果

### 3.5 其他方法

## 4 保证停机

### 4.1 问题

### 4.2 解决方案

### 4.3 选择循环脱离器

### 4.4 其他方案

### 4.5 结果

## 5 整体结构

### 5.1 简化器

### 5.2 出现分析

### 5.3 总结

## 6 三阶段内联策略

### 6.1 为什么采用三阶段

### 6.2 替换

### 6.3 in-scope集合

### 6.4 看看效果

### 6.5 总结

## 7 出现(Occurrence)

### 7.1 替换过程中的查找

### 7.2 在出现场合的内联

### 7.3 对多次出现的变量进行内联

### 7.4 大小很重要

### 7.5 上下文

### 7.6 `Inline` Pragma
