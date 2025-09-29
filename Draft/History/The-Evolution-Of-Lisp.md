# 前言

Lisp is the world’s greatest programming language—or so its proponents think. The structure of Lisp makes it easy to extend the language or even to implement entirely new dialects without starting from scratch. Overall, the evolution of Lisp has been guided more by institutional rivalry, one-upsmanship, and the glee born of technical cleverness that is characteristic of the “hacker culture” than by sober assessments of technical requirements. Nevertheless this process has eventually produced both an industrial-strength programming language, messy but powerful, and a technically pure dialect,small but powerful, that is suitable for use by programming-language theoreticians.

(Lisp 最棒!) ----- 也许只有坚定的lisp厨会这样想。Lisp的结构使得它天生易于扩展或实现一个新方言(但是不需要从头开始)。公允地说，Lisp的进化由直觉性的对抗和试图互相代替的竞争所指导, 其技术层面的智慧结晶，实际上是受所谓“Hacker文化”的浸染较多，而实用导向的需求分析对其影响较小。尽管如此最终这个演变过程仍然产生了一个脏脏但是很能打的工业级方言，顺便也出了一个PL人比较喜欢的，在技术层面上比较“纯粹”的小小方言。

注: 我猜作者暗示的是CommonLisp和Scheme

We pick up where McCarthy’s paper in the first HOPL conference left off. We trace the development chronologically from the era of the PDP-6, through the heyday of Interlisp and MacLisp, past the ascension and decline of special purpose Lisp machines, to the present era of standardization activities. We then examine the technical evolution of a few representative language features, including both some notable successes and some notable failures, that illuminate design issues that distinguish Lisp from other programming languages. We also discuss the use of Lisp as a laboratory for designing other programming languages. We conclude with some reflections on the forces that have driven the evolution of Lisp.

我们从麦卡锡在第一次HOPL会议上发表的论文开始，沿着时间轴对Lisp的进化与演变做一次追踪，起始点为PDP-6时代。我们会见证InterLisp和MacLisp的鼎盛时期，也会看到Lisp-Machine的兴起与没落，接着到达标准化运动的时代。之后我们会展示一点点Lisp语言特性上的技术演进，既包括一些光彩照人的成就，也包括一些令人扼腕的错误，这些鲜明的特征将Lisp和其他语言一分为二。我们也会讨论Lisp作为一个语言设计实验场起到了哪些不容忽视的作用，最后以一些关乎Lisp进化过程中第一驱动力的思考为EOF。

# 2.8 scheme

在1975年秋天时，Sussman和Steele决定做一个MacLisp上的Lisp方言，用于学习Carl Hewitt的Actor模型，因为他们在尝试理解一些paper里的结论时头大了。Sussman学过Algol，所以他提议搞个词法作用域的Lisp方言，亦可赛艇!
Actor使用alpha表示，就像这样, 它没有返回值，只能把计算结果作为message传递给其他actor。以message方式传入的actor被称为continuation。

(define factorial
    (lambda (n)
        (if (= n 0) 1 (* n (factorial (- n 1))))))

(define actorial
    (alpha (n c)
        (if (= n 0) (c 1) (actorial (- n 1) (alpha (f) (c (* f n)))))))

然后在他们检查apply的实现时意外发现其实actor和closure没俩样。
