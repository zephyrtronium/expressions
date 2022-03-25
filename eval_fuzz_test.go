//go:build go1.18
// +build go1.18

package expressions_test

import (
	"math/big"
	"testing"

	"github.com/zephyrtronium/expressions"
)

func FuzzEval(f *testing.F) {
	f.Add("x")
	f.Add("y")
	f.Add("1×2")
	f.Fuzz(func(t *testing.T, s string) {
		expressions.EvalString(s, expressions.SetVar("x", new(big.Float)))
	})
}
