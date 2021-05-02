# expressions

Package expressions provides an arbitrary-precision floating point calculator.

The syntax of expressions is intended to be similar to math you'd write in your notes, with maybe a few more spaces. `2 x y` is a multiplication of three terms. So is `[2]{x}(y)` (although not `2 xy`). `-2^2^n` is the same as `-(2^(2^n))`, where `a^b` is exponentiation.

Variables let you parse an expression once, then evaluate it for many inputs. Or, you can clone contexts to use the same variables for several expressions. You can also define your own functions for the parser and evaluator to use.

As a calculator, expressions uses `*big.Float`. The precision of calculations, intermediate and final, is your choice.


## Grammar

Informally, expressions tries to do what you expect, if you write complicated expressions regularly. You can use round, square, or curly brackets as desired to group subexpressions – `a*(x+y)` is the same as `a×[x+y]` is the same as `a{x+y}`. Exponentiation is done like `a^b`. There are a few predefined constants and elementary functions: `pi`, `e`, `exp`, `log`, `ln`, `sqrt`. (Others, e.g. trig and other elementary functions, would be nice – feel free to [contribute implementations](https://github.com/zephyrtronium/bigfloat)!)

Implicit multiplication (`x y`) is right-associative, whereas explicit multiplication (`x*y` or `x×y`) is left-associative. Function calls with a single unparenthesized argument (`exp x`) parse with the binding level of implicit multiplication. The practical consequence of this is that `exp x y * z` parses the same as `[exp(x*y)] * z`.

Formally, the expression parser implements the following BNF grammar.

```
Expression: AddExpr
AddExpr:    MulExpr | MulExpr '+' MulExpr | MulExpr '-' MulExpr
MulExpr:    Term | Term '*' Term | Term '×' Term
                 | Term '/' Term | Term '÷' Term
Term:       MulExpr | MulExpr Term
UnaryExpr:  PowExpr | '-' UnaryExpr | '+' UnaryExpr
PowExpr:    Primary | Primary '^' PowExpr
Primary:    variable | number | CallExpr | '(' Expression ')'
                     | '[' Expression ']' | '{' Expression '}'
CallExpr:   function | function Term | function ArgList
ArgList:    '(' ')' | '[' ']' | '{' '}'
                    | '(' ExprList ')' | '[' ExprList ']' | '{' ExprList '}'
ExprList:   Expression | Expression ',' ExprList | Expression ';' ExprList
```

`variable` is a terminal consisting of a letter or underscore, followed by any number of letters, underscores or digits.

`function` is a terminal governed by the same rules as `variable`, but specifically one of a fixed set of names given to the parser. The rules for the number of expressions listed in a CallExpr are determined by the function itself at parse time.

`number` is a terminal following the usual rules for base-10 floating point literals in most programming languages, including `e` notation (like `2.1e3` for 2100), with underscores allowed between digits. The first character following a number must not be a letter.
