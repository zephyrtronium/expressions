package expressions

import (
	"io"
	"math/big"
	"strconv"
	"strings"

	"github.com/zephyrtronium/bigfloat"
)

// Context is a context for evaluating expressions. It is not safe to use a
// Context concurrently.
type Context struct {
	stack []*big.Float
	nums  map[string]*big.Float
	names map[string]*big.Float
	prec  uint
	err   error
}

// ContextOption is an option used when creating a context.
type ContextOption interface {
	ctxOption()
}

type (
	varopt struct {
		name string
		val  *big.Float
	}
	varsopt map[string]*big.Float
	precopt uint
)

func (varopt) ctxOption()  {}
func (varsopt) ctxOption() {}
func (precopt) ctxOption() {}

// SetVar sets the value of a variable in the context.
func SetVar(name string, val *big.Float) ContextOption {
	return varopt{name, val}
}

// SetVars sets the values of any number of variables in the context.
func SetVars(vars map[string]*big.Float) ContextOption {
	return varsopt(vars)
}

// Prec sets the precision of calculations.
func Prec(prec uint) ContextOption {
	return precopt(prec)
}

// NewContext creates a new evaluation context. If no precision is given, the
// default is 64.
func NewContext(opts ...ContextOption) *Context {
	ctx := Context{nums: make(map[string]*big.Float), prec: 64}
	return ctx.Clone(opts...)
}

// Eval evaluates an expression returns the result. If an
// error occurs, e.g. a missing variable definition or an argument to a
// function is outside the function's domain, then the result is nil and
// ctx.Err returns the error.
func (ctx *Context) Eval(e *Expr) *big.Float {
	switch len(ctx.stack) {
	case 0: // do nothing
	case 1:
		ctx.stack[0] = new(big.Float).SetPrec(ctx.prec)
		ctx.stack = ctx.stack[:0]
	default:
		panic("expressions: Eval during Eval")
	}
	err := e.n.eval(ctx)
	ctx.err = err
	if err != nil {
		return nil
	}
	return ctx.Result()
}

// Result returns the result obtained after evaluating an expression. Panics if
// ctx has not been used to evaluate an expression. Returns nil if an error
// occurred during evaluation.
func (ctx *Context) Result() *big.Float {
	if ctx.err != nil {
		return nil
	}
	switch len(ctx.stack) {
	case 0:
		panic("expressions: Context.Result called before evaluating any expression")
	case 1:
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

// Set sets the value of a variable. Returns ctx for chaining. Calling Set
// while the context is being used to evaluate an expression panics.
func (ctx *Context) Set(name string, value *big.Float) *Context {
	if len(ctx.stack) > 1 {
		panic("expressions: Set on in-use context")
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
	return new(big.Float).Copy(v)
}

// Prec returns the precision to which values are computed in the context.
func (ctx *Context) Prec() uint {
	return ctx.prec
}

// Clone creates a copy of a context and applies options to it. The returned
// context has no Result and is safe to use to evaluate an expression.
func (ctx *Context) Clone(opts ...ContextOption) *Context {
	n := Context{
		stack: make([]*big.Float, 0, cap(ctx.stack)),
		nums:  make(map[string]*big.Float, len(ctx.nums)),
		names: make(map[string]*big.Float, len(ctx.names)),
		prec:  ctx.prec,
	}
	// First, check for a precision setting. Loop backward so we apply the last
	// precision.
	for i := len(opts) - 1; i >= 0; i-- {
		if p, ok := opts[i].(precopt); ok {
			n.prec = uint(p)
			break
		}
	}
	// Copy numbers only if the new precision is no higher than the old, so
	// that we always use the precision we need.
	if n.prec <= ctx.prec {
		for k, v := range ctx.nums {
			n.nums[k] = new(big.Float).SetPrec(n.prec).Set(v)
		}
	}
	// Copy variables. (We always need a copy in case of Set.) If we have the
	// same precision, we can just copy pointers.
	if n.prec == ctx.prec {
		for name, val := range ctx.names {
			n.names[name] = val
		}
	} else {
		for name, val := range ctx.names {
			n.names[name] = new(big.Float).SetPrec(n.prec).Set(val)
		}
	}
	for _, opt := range opts {
		if opt == nil {
			continue
		}
		switch opt := opt.(type) {
		case varopt:
			n.names[opt.name] = new(big.Float).SetPrec(n.prec).Set(opt.val)
		case varsopt:
			for k, v := range opt {
				n.names[k] = new(big.Float).SetPrec(n.prec).Set(v)
			}
		case precopt:
			// Already done. Do nothing.
		default:
			panic("expressions: unknown option type")
		}
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

// num gets a possibly cached number from its text.
func (ctx *Context) num(s string) *big.Float {
	if r := ctx.nums[s]; r != nil {
		return r
	}
	if s == "∞" {
		s = "inf"
	}
	r, _, err := new(big.Float).SetPrec(ctx.prec).Parse(s, 0)
	switch {
	case err == nil: // do nothing
	case err.Error() == "exponent overflow",
		strings.HasSuffix(err.Error(), ": value out of range"):
		// There isn't realistically any better way to detect this error.
		// N.B. s is non-empty, otherwise we couldn't overflow.
		r = new(big.Float).SetInf(s[0] == '-')
	default:
		panic("expressions: invalid number: " + s + " (" + err.Error() + ")")
	}
	ctx.nums[s] = r
	return r
}

// eval pushes the node's value to the context's stack.
func (n *node) eval(ctx *Context) error {
	switch n.kind {
	case nodeNum:
		ctx.push().Set(ctx.num(n.name))
	case nodeName:
		v := ctx.names[n.name]
		if v == nil {
			return &NameError{Name: n.name}
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
		f := n.fn
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
		// Guard against invalid divisions, 0/0 or inf/inf.
		if l.Sign() == 0 && r.Sign() == 0 || l.IsInf() && r.IsInf() {
			return &DomainError{X: r, Func: "/"}
		}
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
		// Guard against invalid exponentiations, i.e. negative base.
		// TODO: allow negative base with integer exponent
		if l.Signbit() {
			return &DomainError{X: l, Func: "^"}
		}
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

// Eval is a shortcut to parse an expression and return its result using the
// default functions.
func Eval(src io.RuneScanner, opts ...ContextOption) (*big.Float, error) {
	ctx := NewContext(opts...)
	a, err := Parse(src)
	if err != nil {
		return nil, err
	}
	ctx.Eval(a)
	return ctx.Result(), ctx.Err()
}

// EvalString is a shortcut to parse and evaluate a string expression.
func EvalString(src string, opts ...ContextOption) (*big.Float, error) {
	return Eval(strings.NewReader(src), opts...)
}

// NameError is an error from a lookup for a variable that is missing from the
// evaluation context.
type NameError struct {
	// Name is the name that was missing.
	Name string
}

func (err *NameError) Error() string {
	return "undefined variable: " + strconv.Quote(err.Name)
}
