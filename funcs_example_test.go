package expressions_test

import (
	"fmt"
	"math/big"
	"strings"

	"github.com/zephyrtronium/expressions"
)

type nargin struct{}

func (nargin) CanCall(n int) bool {
	return true
}

func (nargin) Call(ctx *expressions.Context, invoc []*big.Float, semis []int, r *big.Float) error {
	r.SetInt64(int64(len(invoc)))
	return nil
}

func ExampleFunc() {
	ctx := expressions.NewContext(nil, map[string]expressions.Func{"nargin": nargin{}}, 32)

	a, _ := expressions.Parse(strings.NewReader("nargin"), ctx)
	b, _ := expressions.Parse(strings.NewReader("nargin 100"), ctx)
	c, _ := expressions.Parse(strings.NewReader("nargin{3, 2, 1}"), ctx)
	fmt.Println(a.Eval(ctx.Clone()), a)
	fmt.Println(b.Eval(ctx.Clone()), b)
	fmt.Println(c.Eval(ctx.Clone()), c)

	// Output:
	// 0 (nargin[])
	// 1 (nargin[(100)])
	// 3 (nargin[(3), (2), (1)])
}
