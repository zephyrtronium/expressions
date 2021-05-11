# expressions

This is an example program showing how to use package expressions as a basic calculator.

## Usage

```
expressions -help
Usage of expressions:
  -echo
        print parse trees
  -fmt string
        result formatting string (default "%g")
  -given string
        name=value variable definition (any number of times)
  -in string
        input file (default stdin if no args given)
  -n    parse separate input lines as separate expressions
  -p int
        precision of calculations in bits (default 64)
```

Arguments after flags are expressions to evaluate. If none are given, the command reads an expression from standard input.

To read from standard input in addition to evaluating argument expressions, use `-` as the input name.

If expressions are specified in both an input file and command arguments, the command uses input files first.

## Examples

Calculate π to 256 bits of precision:

```
$ expressions -p 256 "pi"
3.1415926535897932384626433832795028841971693993751058209749445923078164062862
```

Evaluate a polynomial at several points:

```
$ expressions -given "x=0" -given "y=0" "x^2 + 2 x y - y^2"
0
$ expressions -given "x=1" -given "y=0" "x^2 + 2 x y - y^2"
1
$ expressions -given "x=0" -given "y=1" "x^2 + 2 x y - y^2"
-1
$ expressions -given "x=1" -given "y=1" "x^2 + 2 x y - y^2"
2
```

Check how the polynomial parses:

```
$ expressions -given "x=1" -given "y=1" -echo "x^2 + 2 x y - y^2"
([([x] ^ [2]) + ([2] × [(x) × (y)])] - [(y) ^ (2)]) : 2
```

Evaluate a few expressions:

```
$ echo "x+y" | expressions -given "x=6" -given "y=2" -echo -in=- "x*y" "x^y"
([x] + [y]) : 8
([x] × [y]) : 12
([x] ^ [y]) : 36
```

Evaluate a few expressions from a file:

```
$ echo "1+1" >> ./exprs; echo "2+2" >> ./exprs; echo "1/sqrt 2 pi * exp -x^2" >> ./exprs
$ expressions -given "x=0.5" -in ./exprs -n
2
4
0.31069656037692774486
```

Use an expression to define a variable:

```
$ expressions -given "x=2^8" "x * x"
65536
```
