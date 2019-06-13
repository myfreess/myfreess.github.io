# Feature

**智能补全**

Fish会推导用户的意图,例如:

```shell
function hello --description Hello,users
echo Hello,(whoami)
end

#then
funcsave hel<tab>
-----------------------------------------
hello  (Save function)
help   (Save function)
-----------------------------------------
```
Fish自行推导出了用户的意图(保存函数)，并给出了适合的提示!

这简直就像IDE和EDITOR的区别一样了。

甚至，在使用Make时，fish能补全目标名。

**无变量重拼写**

+ **fish**

```shell
set var 'foo foo'
for i in $var
    echo $i
end
----------------------
foo foo
----------------------
```

+ **Bash**

```shell
var='foo foo'
for i in $var; do
echo $i
done
----------------------
foo
foo
----------------------
```


