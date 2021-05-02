// Package expressions implements an arbitrary-precision floating-point calculator.
//
// The syntax of expressions is intended to be similar to math you'd write in
// your notes, with maybe a few more spaces. "2 x y" is a multiplication of
// three terms. So is "{2}[x](y)" (although not "2 xy"). "-2^2^n" is the same
// as "-(2^(2^n))", where "a^b" is exponentiation.
//
// Variables let you parse an expression once and evaluate it for many inputs,
// or you can clone contexts for several expressions to use the same variable
// definitions everywhere.
//
package expressions
