package expressions_test

import (
	"fmt"
	"math/big"
	"strings"

	exprs "github.com/zephyrtronium/expressions"
)

type nargin struct{}

func (nargin) CanCall(n int) bool {
	return true
}

func (nargin) Call(ctx *exprs.Context, invoc []*big.Float, semis []int, r *big.Float) error {
	r.SetInt64(int64(len(invoc)))
	return nil
}

func ExampleFunc() {
	ctx := exprs.NewContext(exprs.Prec(32))

	a, _ := exprs.Parse(strings.NewReader("nargin"), exprs.ParseFunc("nargin", nargin{}))
	b, _ := exprs.Parse(strings.NewReader("nargin 100"), exprs.ParseFunc("nargin", nargin{}))
	c, _ := exprs.Parse(strings.NewReader("nargin{3, 2, 1}"), exprs.ParseFunc("nargin", nargin{}))
	fmt.Println(a.Eval(ctx), a)
	fmt.Println(b.Eval(ctx), b)
	fmt.Println(c.Eval(ctx), c)

	// Output:
	// 0 (nargin[])
	// 1 (nargin[(100)])
	// 3 (nargin[(3), (2), (1)])
}
