# Calculator
[![CircleCI](https://circleci.com/gh/Abbath/Calculator.svg?style=shield)](https://circleci.com/gh/Abbath/Calculator)

A simple string calculator

Features
--
* Operations `+ - * / % ^`
* Parentheses support
* Scientific number notation support
* Built-in constants `m.pi`, `m.phi` and `m.e`
* Built-in functions `sin cos tan asin acos atan log exp sqrt abs trunc round floor ceil`
* Built-in integral functions `gcd lcm mod div quot rem`
* Ability to print rational numbers using function `prat`
* If conditions with a following syntax `if(<cond_expression>,<expression1>,<expression2>)`
* Lazy evaluation of function arguments
* Comparison functions `lt gt le ge eq ne cmp`
* Comparison operators `< > <= >= == !=`
* Bitwise operators `& |` and functions `xor not`.
* Optimizations like `log(exp(x)) = x`
* Automatic detection of division operation in `atan` argument and `log` function with 2 arguments `log(<base>, <arg>)`
* Variable `_` stores the result of the previous calculation
* A variable can be defined using syntax `<name> = <expression>`
* A user-defined function can be defined using syntax `<name>(<arg1>[,<arg2>[,<arg3>...]]) = <expression>`
* A user-defined operator can be defined using syntax `<name>(<priority>,<associativity>) = <expression>`
* The available symbols for operators are `+-/*%^$!~&|=><`
* Arguments `x `y` are the default for user-defined operators
* A priority of the user-defined operator can be any between `1` and `9`
* Associativity of the user-defined operator is left if `associativity` equals `0` or right otherwise
* A function or operator can take other functions as arguments
* An operator alias can be defined using syntax `<alias_operator> = <operator>`
* Outer variables' values are captured inside user-defined functions
* A signature of the user-defined function consists of a name and arity so `f/0` and `f/1` are different functions 
* Megaparsec backend is available by running with `-bM` or `--backend M` command line flag
* AlexHappy backend is available by running with `-bA` or `--backend A` command line flag
* Web interface can be used by running with `-fW` of `--frontend W` command line flag. The web interface is available on 3000 port
* Comments are supported under Megaparsec backend with syntax `#end comment` and `{inner comment}`
* With Megaparsec backend you shall not define an operator that starts with the same symbols as the operator with a higher priority
* Autocompletion of variables, functions and operators is available on the `Tab` key
* History is available on the `Up` and `Down` keys
* Derivative of a function can be symbolically calculated using syntax `df(<function_expression>, <variable name>)`
* Derivative can be bound to a function alias using syntax `<new_function_name>(<varname>) = df(<function_expression>, <varname>)`
* Numerical integration using adaptive quadrature is available through the function `int` with the following parameters: `int(<function>, <start>, <finish>, <eps>)`
* Random numbers between `0` and `1` via `m.r` constant

Bugs & Todo
--
* The Megaparsec backend has poor support for user-defined operators
* The AlexHappy backend has poor support in general
* Improve the calculation of the derivative
