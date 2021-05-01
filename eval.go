package expressions

import (
	"math/big"
	"strconv"

	"github.com/zephyrtronium/bigfloat"
)

// Context is a context for evaluating expressions. It is not safe to use a
// Context concurrently.
type Context struct {
	stack []*big.Float
	names map[string]*big.Float
	funcs map[string]Func
	prec  uint
	err   error
}

var defaultctx = Context{
	funcs: globalfuncs,
	prec:  64,
}

// NewContext creates a new evaluation context. funcs is a map to merge into
// the default function set; to disable all functions, use DisableDefaultFuncs,
// or to disable individual ones, use a map that sets their names to nil. The
// same set of functions must be used for parsing and evaluation.
func NewContext(names map[string]*big.Float, funcs map[string]Func, prec uint) *Context {
	fns := make(map[string]Func, len(globalfuncs)+len(funcs))
	for k, v := range globalfuncs {
		fns[k] = v
	}
	for k, v := range funcs {
		fns[k] = v
	}
	return &Context{
		names: names,
		funcs: fns,
		prec:  prec,
	}
}

// Result returns the result obtained after evaluating an expression. Panics if
// ctx has not been used to evaluate an expression. Returns nil if an error
// occurred during evaluation.
func (ctx *Context) Result() *big.Float {
	switch len(ctx.stack) {
	case 0:
		panic("expressions: Context.Result called before evaluating any expression")
	case 1:
		if ctx.err != nil {
			return nil
		}
		return ctx.stack[0]
	default:
		panic("expressions: inconsistent stack: " + strconv.Itoa(len(ctx.stack)) + " items (bad AST?)")
	}
}

// Err returns the first error that occurred while evaluating an expression
// with ctx, if any.
func (ctx *Context) Err() error {
	return ctx.err
}

// Set sets the value of a variable. Returns ctx for chaining. Panics if ctx
// has been or is being used to evaluate an expression and has not been reset
// since.
func (ctx *Context) Set(name string, value *big.Float) *Context {
	if len(ctx.stack) != 0 {
		panic("expressions: Context.Set called on used evaluation")
	}
	if ctx.names == nil {
		ctx.names = make(map[string]*big.Float)
	}
	ctx.names[name] = new(big.Float).SetPrec(ctx.prec).Set(value)
	return ctx
}

// Lookup returns a copy of the value of a variable. If there is no such
// variable in the context, then the result is nil.
func (ctx *Context) Lookup(name string) *big.Float {
	v := ctx.names[name]
	if v == nil {
		return nil
	}
	return new(big.Float).SetPrec(ctx.prec).Set(v)
}

// Prec returns the precision to which values are computed in the context.
func (ctx *Context) Prec() uint {
	return ctx.prec
}

// Clone returns a copy of an context, containing the same variables and
// functions and having the same precision.
func (ctx *Context) Clone() *Context {
	n := Context{
		stack: make([]*big.Float, 0, cap(ctx.stack)),
		names: make(map[string]*big.Float, len(ctx.names)),
		funcs: ctx.funcs,
		prec:  ctx.prec,
	}
	for name, val := range ctx.names {
		// Variables are immutable, so we don't need to copy the value.
		n.names[name] = val
	}
	return &n
}

// push ensures a settable value on the stack.
func (ctx *Context) push() *big.Float {
	if len(ctx.stack) < cap(ctx.stack) {
		ctx.stack = ctx.stack[:len(ctx.stack)+1]
		if ctx.stack[len(ctx.stack)-1] == nil {
			ctx.stack[len(ctx.stack)-1] = new(big.Float).SetPrec(ctx.prec)
		}
	} else {
		ctx.stack = append(ctx.stack, new(big.Float).SetPrec(ctx.prec))
	}
	return ctx.stack[len(ctx.stack)-1]
}

// pop removes the top from the stack and returns it. The returned value may be
// modified by future node evaluations.
func (ctx *Context) pop() *big.Float {
	r := ctx.stack[len(ctx.stack)-1]
	ctx.stack = ctx.stack[:len(ctx.stack)-1]
	return r
}

// top is a shortcut to get the top element of the stack.
func (ctx *Context) top() *big.Float {
	return ctx.stack[len(ctx.stack)-1]
}

// eval pushes the node's value to the context's stack.
func (n *node) eval(ctx *Context) error {
	switch n.kind {
	case nodeNum:
		ctx.push().Set(n.num)
	case nodeName:
		v := ctx.names[n.name]
		if v == nil {
			return &NameError{Name: n.name, Func: false}
		}
		ctx.push().Set(v)
	case nodeCall:
		r := ctx.push()
		k := len(ctx.stack)
		var semis []int
		i := 0
		for l := n.right; l != nil; l = l.right {
			if err := l.left.eval(ctx); err != nil {
				return err
			}
			if l.name == ";" {
				semis = append(semis, i)
			}
			i++
		}
		f := ctx.funcs[n.name]
		if f == nil {
			return &NameError{Name: n.name, Func: true}
		}
		invoc := ctx.stack[k:len(ctx.stack):len(ctx.stack)]
		if err := f.Call(ctx, invoc, semis, r); err != nil {
			return err
		}
		ctx.stack = ctx.stack[:k]
	case nodeArg:
		panic("expressions: eval on nodeArg")
	case nodeNeg:
		if err := n.left.eval(ctx); err != nil {
			return err
		}
		v := ctx.stack[len(ctx.stack)-1]
		v.Neg(v)
	case nodeAdd:
		if err := n.left.eval(ctx); err != nil {
			return err
		}
		if err := n.right.eval(ctx); err != nil {
			return err
		}
		r := ctx.pop()
		l := ctx.top()
		l.Add(l, r)
	case nodeSub:
		if err := n.left.eval(ctx); err != nil {
			return err
		}
		if err := n.right.eval(ctx); err != nil {
			return err
		}
		r := ctx.pop()
		l := ctx.top()
		l.Sub(l, r)
	case nodeMul:
		if err := n.left.eval(ctx); err != nil {
			return err
		}
		if err := n.right.eval(ctx); err != nil {
			return err
		}
		r := ctx.pop()
		l := ctx.top()
		l.Mul(l, r)
	case nodeDiv:
		if err := n.left.eval(ctx); err != nil {
			return err
		}
		if err := n.right.eval(ctx); err != nil {
			return err
		}
		r := ctx.pop()
		l := ctx.top()
		// TODO: check for Quo panic
		l.Quo(l, r)
	case nodePow:
		if err := n.left.eval(ctx); err != nil {
			return err
		}
		if err := n.right.eval(ctx); err != nil {
			return err
		}
		r := ctx.pop()
		l := ctx.top()
		// TODO: check for Pow panic
		bigfloat.Pow(l, l, r)
	case nodeNop:
		if err := n.left.eval(ctx); err != nil {
			return err
		}
	default:
		panic("expressions: invalid AST node " + n.kind.String())
	}
	return nil
}

// NameError is an error from a lookup for a variable or
// function that is missing from the evaluation context.
type NameError struct {
	// Name is the name that was missing.
	Name string
	// Func is whether the name is for a function.
	Func bool
}

func (err *NameError) Error() string {
	if err.Func {
		return "undefined function: " + strconv.Quote(err.Name)
	}
	return "undefined variable: " + strconv.Quote(err.Name)
}
