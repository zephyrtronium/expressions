//go:build go1.18
// +build go1.18

package expressions_test

import (
	"strings"
	"testing"

	"github.com/zephyrtronium/expressions"
)

func FuzzParse(f *testing.F) {
	f.Add("x")
	f.Add("y")
	f.Add("1×2")
	f.Fuzz(func(t *testing.T, s string) {
		expressions.Parse(strings.NewReader(s))
	})
}
