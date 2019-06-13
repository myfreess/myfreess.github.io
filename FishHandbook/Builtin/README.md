# Builtin

Fish shell丢弃了Bash中的shell关键字，转而用内建命令实现条件分支，循环，布尔运算……

变量赋值，也依赖于内建命令`set`。


**set**

```shell
#常规字符串变量
set Flag someting
echo $Flag

#环境变量
set -x Flag something
fish -c 'echo $Flag'

#将变量限制在if等内置命令的控制范围内
for our in nobody
      set -l Flag someting
      echo $Flag
end
echo $Flag

#跳出限制
for our in nobody
      set -g Flag someting
      echo $Flag
end
echo $Flag

#在不同的fish进程中分享变量
set -U Flag somthing
#打开一个新的fish进程
fish
echo $Flag
#或者
fish -c 'echo $Flag'

#展示所有变量
set

#上一进程退出码
echo $status

#自定义PATH
set -U fish_user_paths $fish_user_paths /system/bin
#将/system/bin加入PATH
#当切换到bash时，上述更改不生效。

#将命令输出替换为变量
set Flag (echo something)
echo $Flag
#当变量赋值时，命令已经被执行
set Flag (sleep 10)

#消除一个变量
set Flag something
echo $Flag
set -e Flag
echo $Flag
```

set支持数组,不过编号是从1开始的。

```shell
set evil vi emacs
#名为evil的数组被建立

echo $evil[1]
-------------
vi
------------

echo $evil[2]
------------
emacs
------------

set evil[1] emacs
set evil[2] vi

#fish不支持关联数组

#set forest[tree] pine
#set forest[fruit] apple

#奇特，在fish中
#可以有两种方式取出变量值
#不如说，fish变量只有数组
set var foo
echo $var
echo $var[1]
```

**if/switch**

fish引入了switch，反正esac不用再写了。

```shell
if [ (whoami) = root ]
   echo "Hello,Adminstrator"
else
   echo "Hello,(whoami)"
end


if [ -x (which bash) ]
   echo "Breakdown Again Shell"
else if [ -x (which fish) ]
     echo "Friendly interactive shell"
end

set object 0
switch $object
       case number
       echo 数字
       case binary
       echo 可执行文件
       case '*'
       echo 未知
end
#不加单引号case后的*会被展开

```
**for/while**

没有until。

```shell
for i in 1 2 3
    set i (expr $i + 1)
    echo $i
    if [ $i -eq 3 ]
       break
    end
end
set -e i

while true
      echo $status
end
#等同于
yes 0
```

**function/funcsave**

```shell
function hello
	 echo "Hello,World"
end

#执行函数
eval "hello"

#将此函数保存到文件中
funcsave hello
```

fish不支持`$1`,`$2`，只有一个`$argv`代表所有参数。但是，可用$argv[1]代替$1。

**and/or**

```shell
echo "Hello,Trouble."; and echo "Trouble,不要再来了!"

false; or echo "Dont be evil"

#and代表&&

#or代表||

#&&和||也是兼容的

```

**count**

真的就只能计算参数数量

```shell
count pine bird apple #stdout3
```

当无参数时返回值($status)为1,标准输出为0。

双引号内的内容只算一个参数。

```shell
count "1 2 3" #stdout 1
```

单引号也是，所以count $argv时,别用双引号包围$argv。

**command**

强制执行一个外部程序，忽略同名function和builtin。




