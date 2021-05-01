package expressions_test

import (
	"errors"
	"fmt"
	"math"
	"math/big"
	"reflect"
	"regexp"
	"strings"
	"testing"

	"github.com/zephyrtronium/expressions"
)

func TestEval(t *testing.T) {
	type vv struct {
		n string
		v float64
	}
	type vc struct {
		vars []vv
		r    float64
	}
	cases := []struct {
		name string
		src  string
		r    []vc
	}{
		{"num", "1", []vc{{nil, 1}}},
		{"ident", "x", []vc{
			{[]vv{{"x", 4}}, 4},
			{[]vv{{"x", 5}}, 5},
			{[]vv{{"x", 6}}, 6},
		}},
		{"plus", "+x", []vc{
			{[]vv{{"x", 4}}, 4},
			{[]vv{{"x", 5}}, 5},
			{[]vv{{"x", 6}}, 6},
		}},
		{"neg", "-x", []vc{
			{[]vv{{"x", 4}}, -4},
			{[]vv{{"x", 5}}, -5},
			{[]vv{{"x", 6}}, -6},
		}},
		{"add", "4+5+6", []vc{{nil, 4 + 5 + 6}}},
		{"sub", "4-5-6", []vc{{nil, 4 - 5 - 6}}},
		{"mul", "4*5*6", []vc{{nil, 4 * 5 * 6}}},
		{"div", "4/5/6", []vc{{nil, 4.0 / 5.0 / 6.0}}},
		{"pow", "4^3^2", []vc{{nil, 262144}}},
		{"pi", "pi", []vc{{nil, math.Pi}}},
		{"e", "e", []vc{{nil, math.E}}},
		{"exp", "exp 1", []vc{{nil, math.E}}},
	}
	ctx := expressions.NewContext(nil, nil, 64)
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			a, err := expressions.Parse(strings.NewReader(c.src), ctx)
			if err != nil {
				t.Fatal(c.src, "failed to parse:", err)
			}
			for _, v := range c.r {
				ctx := ctx.Clone()
				for _, x := range v.vars {
					ctx.Set(x.n, new(big.Float).SetFloat64(x.v))
				}
				r := a.Eval(ctx)
				if ctx.Err() != nil {
					t.Error("evaluation error:", ctx.Err())
				}
				if r == nil {
					t.Fatal("nil result")
				}
				if q := ctx.Result(); r.Cmp(q) != 0 {
					t.Errorf("different results: Eval returned %g, Result returned %g", r, q)
				}
				if f, _ := r.Float64(); f != v.r {
					t.Errorf("wrong result: want %g, got %g", v.r, r)
				}
			}
		})
	}
}

func TestEvalUndefNames(t *testing.T) {
	cases := []struct {
		name string
		src  string
		r    []string
	}{
		{"x", "x", []string{"x"}},
		{"plus", "+x", []string{"x"}},
		{"neg", "-x", []string{"x"}},
		{"add-lhs", "x+1", []string{"x"}},
		{"add-rhs", "1+x", []string{"x"}},
		{"sub-lhs", "x-1", []string{"x"}},
		{"sub-rhs", "1-x", []string{"x"}},
		{"mul-lhs", "x*1", []string{"x"}},
		{"mul-rhs", "1*x", []string{"x"}},
		{"div-lhs", "x/1", []string{"x"}},
		{"div-rhs", "1/x", []string{"x"}},
		{"pow-lhs", "x^1", []string{"x"}},
		{"pow-rhs", "1^x", []string{"x"}},
		{"call", "exp(x)", []string{"x"}},
	}
	ure := regexp.MustCompile(`(?i)\bundef`)
	vre := regexp.MustCompile(`(?i)\bvar`)
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			ctx := expressions.NewContext(nil, nil, 64)
			a, err := expressions.Parse(strings.NewReader(c.src), ctx)
			if err != nil {
				t.Fatalf("%q failed to parse: %v", c.src, err)
			}
			if v := a.Vars(); !reflect.DeepEqual(c.r, v) {
				t.Errorf("%q gave wrong variables: want %q, got %q", c.src, c.r, v)
			}
			if r := a.Eval(ctx); r != nil {
				t.Errorf("evaluating %q gave non-nil result %g", c.src, r)
			}
			err = ctx.Err()
			if ctx.Err() == nil {
				t.Fatalf("evaluating %q gave no error", c.src)
			}
			u, ok := err.(*expressions.NameError)
			if !ok {
				t.Fatalf("error was %#v, not NameError", err)
			}
			msg := err.Error()
			if !ure.MatchString(msg) {
				t.Errorf(`%q doesn't mention "undef"`, msg)
			}
			if !vre.MatchString(msg) {
				t.Errorf(`%q doesn't mention "var"`, msg)
			}
			for _, v := range c.r {
				if v == u.Name {
					xre := regexp.MustCompile(`\b` + v + `\b`)
					if !xre.MatchString(msg) {
						t.Errorf(`%q doesn't mention %q`, msg, v)
					}
					return
				}
			}
			t.Errorf("NameError on %q, not in %q", u.Name, c.r)
		})
	}
}

func TestEvalFuncError(t *testing.T) {
	cases := []struct {
		name string
		src  string
	}{
		{"sqrt", "sqrt(-1)"},
		{"log", "log(-1)"},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			ctx := expressions.NewContext(nil, nil, 64)
			a, err := expressions.Parse(strings.NewReader(c.src), ctx)
			if err != nil {
				t.Fatalf("%q failed to parse: %v", c.src, err)
			}
			if r := a.Eval(ctx); r != nil {
				t.Errorf("evaluating %q gave non-nil result %g", c.src, r)
			}
			err = ctx.Err()
			if ctx.Err() == nil {
				t.Fatalf("evaluating %q gave no error", c.src)
			}
			switch {
			case !errors.As(err, new(big.ErrNaN)): // do nothing
			case !errors.As(err, new(expressions.DomainError)): // do nothing
			default:
				t.Errorf("%#v is neither *big.ErrNaN nor *expressions.DomainError", err)
			}
		})
	}
}

func TestContextVars(t *testing.T) {
	zero := new(big.Float)
	one := new(big.Float).SetFloat64(1)
	ctx := expressions.NewContext(map[string]*big.Float{"x": zero}, nil, 64)
	if x := ctx.Lookup("x"); x == nil || x.Cmp(zero) != 0 {
		t.Errorf("x should be %[1]v at %[1]p but is %[2]v at %[2]p", zero, x)
	}
	if y := ctx.Lookup("y"); y != nil {
		t.Errorf("context has y: %[1]v at %[1]p", y)
	}
	ctx.Set("y", one)
	if x := ctx.Lookup("x"); x == nil || x.Cmp(zero) != 0 {
		t.Errorf("x should be %[1]v at %[1]p but is %[2]v at %[2]p", zero, x)
	}
	if y := ctx.Lookup("y"); y == nil || y.Cmp(one) != 0 {
		t.Errorf("y should be %[1]v at %[1]p but is %[2]v at %[2]p", one, y)
	}
	ctx.Set("x", one)
	if x := ctx.Lookup("x"); x == nil || x.Cmp(one) != 0 {
		t.Errorf("x should be %[1]v at %[1]p but is %[2]v at %[2]p", one, x)
	}
	if y := ctx.Lookup("y"); y == nil || y.Cmp(one) != 0 {
		t.Errorf("y should be %[1]v at %[1]p but is %[2]v at %[2]p", zero, y)
	}
}

func Example() {
	var (
		fx   = strings.NewReader("x^3/2 - x")
		dfx  = strings.NewReader("3 x^2/2 - 1")
		ddfx = strings.NewReader("3 x")
	)
	ctx := expressions.NewContext(nil, nil, 64)
	a, _ := expressions.Parse(fx, ctx)
	b, _ := expressions.Parse(dfx, ctx)
	c, _ := expressions.Parse(ddfx, ctx)

	for i := 0; i < 4; i++ {
		x := big.NewFloat(float64(i))
		ctx := ctx.Set("x", x)
		y := a.Eval(ctx.Clone())
		yp := b.Eval(ctx.Clone())
		ypp := c.Eval(ctx.Clone())
		fmt.Printf("x = %g   y = %-4g  y' = %-4g  y'' = %g\n", x, y, yp, ypp)
	}

	// Output:
	// x = 0   y = 0     y' = -1    y'' = 0
	// x = 1   y = -0.5  y' = 0.5   y'' = 3
	// x = 2   y = 2     y' = 5     y'' = 6
	// x = 3   y = 10.5  y' = 12.5  y'' = 9
}
