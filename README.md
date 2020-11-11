# Calculator
[![Build Status](https://travis-ci.org/Abbath/Calculator.svg?branch=master)](https://travis-ci.org/Abbath/Calculator)
[![CircleCI](https://circleci.com/gh/Abbath/Calculator.svg?style=shield)](https://circleci.com/gh/Abbath/Calculator)
[![Code Climate](https://codeclimate.com/github/Abbath/Calculator/badges/gpa.svg)](https://codeclimate.com/github/Abbath/Calculator)

A simple string calculator

Features
--
* Operations `+ - * / % ^`
* Parentheses support
* Scientific number notation support
* Embedded constants `pi` and `e`
* Embedded functions `sin cos tan asin acos atan log exp sqrt abs`
* Embedded integral functions `gcd lcm mod div quot rem`
* Ability to print rational numbers using function `prat`
* If conditions with a following syntax `if(<cond_expression>,<expression1>,<expression2>)`
* Lazy evaluation of function arguments
* Comparison functions `lt gt le ge eq ne`
* Comparison operators `< > <= >= == !=`
* Bitwise operators `& |` and functions `xor not`.
* Optimizations like `log(exp(x)) = x`
* Automatic detection of division operation in `atan` argument
* Variable `_` stores result of the previous calculation
* A variable can be defined using syntax `<name> = <expression>`
* A user defined function can be defined using syntax `<name>(<arg1>[,<arg2>[,<arg3>...]]) = <expression>`
* A user defined operator can be defined using syntax `<name>(<priority>,<associativity>) = <expression>`
* The available symbols for operators are `+-/*%^$!~&|=><`
* Arguments `x y` are default for user defined operators
* A priority of the user defined operator can be any between `1` and `6`
* An associativity of the user defined operator is left if `associativity` equals `0` or right otherwise
* A function or operator can take other functions as arguments
* An operator alias can be defined using syntax `<alias_operator> = <operator>`
* Outer variables' values are captured inside user defined functions
* A signature of the user defined function consists of a name and arity so `f/0` and `f/1` are different functions 
* Megaparsec frontend is available by running with `-m` or `--megaparsec-frontend` command line flag
* AlexHappy frontend is available by running with `-x` or `--alex-happy-frontend` command line flag
* Web interface can be used by running with `-w` of `--web` command line flag. Web interface is available on 3000 port
* Comments are supported under Megaparsec frontend with syntax `#end comment` and `{inner comment}`
* With Megaparsec frontend you shall not define operator that starts with the same symbols as the operator with higher priority
* Autocompletion of internal functions and operators is available on the `Tab` key
* History is available on the `Up` and `Down` keys
* Derivative of a function can be symbolically calculated using syntax `df(<function_expression>, <variable name>)`
* Derivative can be binded to a function alias using syntax `<new_function_name>(<varname>) = df(<function_expression>, <varname>)`
* Numerical integration is available through the function `int` with the following parameters: `int(<function>, <start>, <finish>, <step>)`

Bugs & Todo
--
* Megaparsec frontend has poor support of user defined operators
* AlexHappy frontend has poor support in general
* Improve the derivatives calculation
