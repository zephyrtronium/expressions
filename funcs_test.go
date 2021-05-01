package expressions

import (
	"math/big"
)

type mockfn struct {
	can []int
}

func mockFunc(n ...int) Func {
	return mockfn{can: n}
}

func (f mockfn) Call(ctx *Context, invoc []*big.Float, semis []int, r *big.Float) error {
	return nil
}

func (f mockfn) CanCall(n int) bool {
	for _, v := range f.can {
		if v == n {
			return true
		}
	}
	return false
}

var testfns = map[string]Func{
	"zero":    mockFunc(0),
	"one":     mockFunc(1),
	"zeroone": mockFunc(0, 1),
	"five":    mockFunc(5),
	// disable e to make writing tests a bit easier
	"e": nil,
}
