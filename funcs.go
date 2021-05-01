package expressions

import (
	"errors"
	"math/big"
	"strconv"

	"github.com/zephyrtronium/bigfloat"
)

// TODO(zeph): how represent e.g. 0F0(; ; z) = exp(z)

// Func is a function from reals to reals. Functions may but generally should
// not look up variables. The function should set r to its result and should
// not use the value of r otherwise.
type Func interface {
	// Call evaluates the function. The function arguments are passed in invoc.
	// semis is the indices of arguments which are preceded by semicolons.
	// The function may but generally should not look up variables. The
	// function must set r to its result and should not use the value of r
	// otherwise. invoc has a length for which CanCall returned true. Call may
	// modify the elements of invoc.
	Call(ctx *Context, invoc []*big.Float, semis []int, r *big.Float) error

	// CanCall returns whether the function can be called with n arguments.
	// This controls how the expression parser handles instances of this
	// function:
	//
	// 	1.	If a bracketed list of n > 0 expressions follows a function, the
	//		parser treats it as an argument list if CanCall(n). (If n is 1 and
	//		!CanCall(1) and CanCall(0), then the list is a multiplication;
	//		otherwise, it is rejected.)
	//
	// 	2.	If a bare term follows a function and CanCall(1), then the parser
	//		treats the term as an argument to the function. E.g., "exp x" is
	//		parsed as "exp(x)". (If !CanCall(1), then it is a multiplication.)
	CanCall(n int) bool
}

var globalfuncs = map[string]Func{
	"exp": Monadic(bigfloat.Exp),
	"ln":  Monadic(bigfloat.Log),
	"log": Monadic(func(out, in *big.Float) *big.Float {
		bigfloat.Log(out, in)
		in.SetFloat64(10).SetPrec(out.Prec())
		bigfloat.Log(in, in)
		return out.Quo(out, in)
	}),
	"sqrt": Monadic((*big.Float).Sqrt),

	// trig, not yet implemented in dependencies
	"cos":   nil,
	"sin":   nil,
	"tan":   nil,
	"acos":  nil,
	"asin":  nil,
	"atan":  nil,
	"cosh":  nil,
	"sinh":  nil,
	"tanh":  nil,
	"acosh": nil,
	"asinh": nil,
	"atanh": nil,

	// constants
	"pi": Niladic(bigfloat.Pi),
	"e": Niladic(func(out *big.Float) *big.Float {
		var one big.Float
		one.SetFloat64(1)
		return bigfloat.Exp(out, &one)
	}),
}

// DisableDefaultFuncs returns a functions map suitable for disabling all
// default functions when passed as the function set for NewContext.
func DisableDefaultFuncs() map[string]Func {
	m := make(map[string]Func, len(globalfuncs))
	for k := range globalfuncs {
		m[k] = nil
	}
	return m
}

type monadic struct {
	f func(out, in *big.Float) *big.Float
}

func (m monadic) Call(ctx *Context, invoc []*big.Float, semis []int, r *big.Float) (err error) {
	in := invoc[0]
	defer func() {
		r := recover()
		if r == nil {
			return
		}
		err = r.(error) // panic if not error
		if errors.As(err, &DomainError{}) || errors.As(err, &big.ErrNaN{}) {
			return
		}
		panic(err)
	}()
	r.SetPrec(ctx.Prec())
	m.f(r, in)
	return nil
}

func (m monadic) CanCall(n int) bool {
	return n == 1
}

// Monadic wraps a function of one variable into a Func. f must set out to its
// result, to the precision of in; its return value is always ignored. If f is
// called on an argument outside f's domain, it should panic with an error of
// type big.ErrNaN, or that unwraps to it.
func Monadic(f func(out, in *big.Float) *big.Float) Func {
	return monadic{f}
}

type niladic struct {
	f func(out *big.Float) *big.Float
}

func (n niladic) Call(ctx *Context, invoc []*big.Float, semis []int, r *big.Float) (err error) {
	r.SetPrec(ctx.Prec())
	n.f(r)
	return nil
}

func (n niladic) CanCall(k int) bool {
	return k == 0
}

// Niladic wraps a function of zero variables, generally a function which
// computes a constant, into a Func. f must set out to its result; its return
// value is always ignored. Unlike Monadic, the wrapped function is expected
// never to panic.
func Niladic(f func(out *big.Float) *big.Float) Func {
	return niladic{f}
}

// DomainError is an error returned when a function is called on arguments
// outside its domain. DomainError unwraps to big.ErrNaN.
type DomainError struct {
	// X is the out-of-domain argument.
	X *big.Float
	// Arg is the 1-based index of the argument.
	Arg int
	// Func is a name identifying the function.
	Func string
}

func (err DomainError) Error() string {
	r := err.X.String() + " outside domain"
	if err.Func != "" {
		r += " of " + err.Func
	}
	if err.Arg > 0 {
		r += " (argument " + strconv.Itoa(err.Arg) + ")"
	}
	return r
}
