# Calculator
[![CircleCI](https://circleci.com/gh/Abbath/Calculator.svg?style=shield)](https://circleci.com/gh/Abbath/Calculator)

A simple string calculator

Features
--
* Operations `+ - * / % ^`
* Parentheses support
* Scientific number notation support
* Complex numbers support via `j` or `i` infix notation: `1j1` or `1i1`
* Underscores can be used in numbers: `1_000_000`
* Built-in constants `m.pi`, `m.phi`, `m.e`, `b.true` and `b.false`
* Locally unchangeable constants with a `c.` prefix: `c.x`
* Built-in functions `sin cos tan asin acos atan cot sec csc coth sech csch acot asec acsc acoth asech acsch sinc cosc sincn coscn log exp sqrt abs trunc round floor ceil`
* Built-in integral functions `gcd lcm mod div quot rem`
* Ability to print rational numbers using function `prat`
* If conditions with a following syntax `if(<cond_expression>,<expression1>,<expression2>)`
* Loops using the `loop` function: `loop(<cond_expression>, <expression>)` or `loop(<init_expression>, <cond_expression>, <expression>)`
* Lazy evaluation of function arguments
* Comparison functions `lt gt le ge eq ne cmp`
* Comparison operators `< > <= >= == !=`
* Bitwise operators `& | << >> ~` and functions `xor not`
* `pop` and `comp` functions for the calculation of pop count and complement respectively
* Optimizations like `log(exp(x)) = x`
* Automatic detection of division operation in `atan` argument and `log` function with 2 arguments `log(<base>, <arg>)`
* Variable `_` stores the result of the previous calculation
* A variable can be defined using syntax `<name> = <expression>`
* A user-defined function can be defined using syntax `<name>(<arg1>[,<arg2>[,<arg3>...]]) = <expression>`
* A user-defined operator can be defined using syntax `<name>(<precedence>,<associativity>) = <expression>`
* The available symbols for operators are `+-/*%^$!~&|=><:`
* Arguments `x` and `y` are the default for user-defined operators
* The precedence of the user-defined operator can be any between `1` and `14`
* Associativity of the user-defined operator is left if `associativity` equals `0` or right otherwise
* A function or operator can take other functions as arguments
* An operator alias can be defined using syntax `<alias_operator> = <operator>`
* Outer variables' values are captured inside user-defined functions
* A signature of the user-defined function consists of a name and arity so `f/0` and `f/1` are different functions 
* Megaparsec backend is available by running with `-bM` or `--backend M` command line flag
* AlexHappy backend is available by running with `-bA` or `--backend A` command line flag
* The web interface can be used by running with `-fW` of `--frontend W` command line flag. The web interface is available on 3000 port
* Comments are supported under Megaparsec backend with syntax `#end comment` and `{inner comment}`
* With Megaparsec backend you shall not define an operator that starts with the same symbols as the operator with a higher precedence
* Autocompletion of variables, functions and operators is available on the `Tab` key
* History is available on the `Up` and `Down` keys
* Derivative of a function can be symbolically calculated using syntax `df(<function_expression>, <variable name>)`
* Derivative can be bound to a function alias using syntax `<new_function_name>(<varname>) = df(<function_expression>, <varname>)`
* Numerical integration using adaptive quadrature is available through the function `int` with the following parameters: `int(<function>, <start>, <finish>, <eps>)`
* Random numbers between `0` and `1` via `m.r` constant
* "Strings" delimited by `"`. Can't do anything with those strings though. Function `str(<expression>)` displays a value as a string
* `fmt(<format_string>, <args...>)` function for C-style formatting. Use `%s` for a string and `%f` for a number
* Local assignment operators `:=` and `::=` for creating binding visible in the same line only 
* Colon operator `:` for sequential computing of expressions
* Pipe operator `|>` for piping the computation: `value |> f1 |> f2 |> f3`
* Functions can be used as infix operators by surrounding with backticks "`"
* Aliases top-level operators `+= -= *= /= %= ^= |= &=` which unsugar into: `x <op>= value` -> `x = x <op> value`

Bugs & Todo
--
* The Megaparsec backend is broken
* The AlexHappy backend has poor support in general
* Improve the calculation of the derivative
