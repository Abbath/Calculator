# Calculator

A simple string calculator

Features
--
* Operations `+ - * / % ^`
* Parentheses support
* Scientific number notation support
* Embedded constants `pi` and `e`
* Embedded functions `sin cos tan asin acos atan log exp sqrt abs`
* If conditions with a following syntax `if(<cond_expression>,<expression1>,<expression2>)`
* Lazy evaluation of function arguments
* Condition functions `lt gt le ge eq ne`
* Optimisations like `log(exp(x) = x`
* Automatic detection of division operation in `atan` argument
* Variable `_` stores result of the previous calculation
* A variable can be defined using syntax `<name> = <expression>`
* A user defined function can be defined using syntax `<name>(<arg1>[,<arg2>[,<arg3>...]]) = <expression>`
* Outer variables' values are captured inside user defined functions
* A signature of the user defined function consists of a name and arity
