package expressions

import (
	"fmt"
	"reflect"
	"regexp"
	"strings"
	"testing"
)

// diff finds the first in-order node of n that differs from m, or nil, nil if
// the two ASTs are equal. If any node is nodeNone, it is returned.
func (n *node) diff(m *node) (*node, *node) {
	if n == nil {
		if m != nil {
			return n, m
		}
		return nil, nil
	}
	if m == nil {
		return n, m
	}
	if n.kind == nodeNone || m.kind == nodeNone {
		return n, m
	}
	if n.kind != m.kind {
		return n, m
	}
	switch n.kind {
	case nodeNum:
		if n.num.Cmp(m.num) != 0 {
			return n, m
		}
	case nodeName:
		if n.name != m.name {
			return n, m
		}
	case nodeCall:
		if n.name != m.name {
			return n, m
		}
		if d, e := n.right.diff(m.right); d != nil || e != nil {
			return d, e
		}
	case nodeArg, nodeNeg, nodeAdd, nodeSub, nodeMul, nodeDiv, nodePow:
		if d, e := n.left.diff(m.left); d != nil || e != nil {
			return d, e
		}
		if d, e := n.right.diff(m.right); d != nil || e != nil {
			return d, e
		}
	case nodeNop:
		if d, e := n.left.diff(m.left); d != nil || e != nil {
			return d, e
		}
	default:
		panic(fmt.Errorf("invalid node kind: n=%+v m=%+v", n, m))
	}
	return nil, nil
}

func TestOpPrecsExist(t *testing.T) {
	for _, r := range Operators {
		_, b := binops[string(r)]
		_, u := preops[string(r)]
		if !b && !u {
			t.Errorf("no operator for %c", r)
		}
	}
}

func TestTermPrecMatchesMultiplication(t *testing.T) {
	if p := binops["*"].prec; p != termprec.prec {
		t.Errorf("terms have prec %d but * has prec %d", termprec.prec, p)
	}
	if p := binops["×"].prec; p != termprec.prec {
		t.Errorf("terms have prec %d but × has prec %d", termprec.prec, p)
	}
}

func TestParseTrees(t *testing.T) {
	cases := []struct {
		name string
		a, b string
	}{
		{"paren", "(x)", "x"},
		{"square", "[x]", "x"},
		{"curly", "{x}", "x"},
		{"multi", "([{{[((x))]}}])", "x"},

		{"plus", "+x", "(+(x))"},
		{"neg", "-x", "(-(x))"},
		{"negnum", "-1", "(-(1))"},
		{"add", "x+y", "((x)+(y))"},
		{"sub", "x-y", "((x)-(y))"},
		{"mul", "x*y", "((x)*(y))"},
		{"div", "x/y", "((x)/(y))"},
		{"pow", "x^y", "((x)^(y))"},
		{"altmul", "x×y", "x*y"},
		{"altdiv", "x÷y", "x/y"},
		{"terms", "x y", "x*y"},
		{"parenterms", "x(y)", "x*y"},

		{"call0", "zero()", "zero"},
		{"call0-terms", "zero x", "zero()*x"},
		{"call0-paren", "zero(x)", "zero()*x"},
		{"call1-bare", "one x", "one(x)"},
		{"call1-terms", "one a b c * d", "one(a b c) * d"},
		{"call1-plus", "one + x", "one(+x)"},
		{"call1-add", "one x + y", "one(x) + y"},
		{"call1-exp", "one x^y", "one(x^y)"},
		{"call5", "five(a; b; c; d; e)", "five(a, b, c, d, e)"},

		{"add4", "w+x+y+z", "((w+x)+y)+z"},
		{"sub4", "w-x-y-z", "((w-x)-y)-z"},
		{"mul4", "w*x*y*z", "((w*x)*y)*z"},
		{"div4", "w/x/y/z", "((w/x)/y)/z"},
		{"pow4", "w^x^y^z", "w^(x^(y^z))"},
		{"terms4", "w x y z", "w*(x*(y*z))"},

		{"negpow", "-1^n", "-(1^n)"},
		{"desc", "w^x*y+z", "((w^x)*y)+z"},
		{"asc", "w+x*y^z", "w+(x*(y^z))"},
		{"descasc", "w^x*y+z+a*b^c", "(((w^x)*y)+z)+a*(b^c)"},
		{"ascdesc", "w+x*y^z^a*b+c", "w+((x*(y^(z^a)))*b)+c"},
		{"negneg", "--x", "-(-x)"},
		{"negsub", "-x-x", "(-x)-x"},
		{"powparen", "x^y(z)", "(x^y)*z"},
		{"powneg", "x^-1", "x^(-1)"},
		{"powterms", "x y^z", "x*(y^z)"},
		{"pownegpow", "x^-y^-z", "x^(-(y^(-z)))"},
		{"pownegneg", "x^--y", "x^(-(-y))"},
	}
	ctx := NewContext(nil, testfns, 64)
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			a, err := Parse(strings.NewReader(c.a), ctx)
			if err != nil {
				t.Fatalf("failed to parse %q: %v", c.a, err)
			}
			b, err := Parse(strings.NewReader(c.b), ctx)
			if err != nil {
				t.Fatalf("failed to parse %q: %v", c.b, err)
			}
			d, e := a.n.diff(b.n)
			if d != nil || e != nil {
				t.Errorf("mismatched AST:\n\t%q parses %v has %v\n\t%q parses %v has %v", c.a, a.n, d, c.b, b.n, e)
			}
		})
	}
}

func TestParseExact(t *testing.T) {
	cases := []struct {
		name string
		src  string
		n    *node
	}{
		{
			name: "call01-paren",
			src:  "zeroone(x)",
			n: &node{
				kind: nodeCall,
				name: "zeroone",
				right: &node{
					kind: nodeArg,
					name: "",
					left: &node{
						kind: nodeName,
						name: "x",
					},
				},
			},
		},
		{
			name: "call1-paren",
			src:  "one(x)",
			n: &node{
				kind: nodeCall,
				name: "one",
				right: &node{
					kind: nodeArg,
					name: "",
					left: &node{
						kind: nodeName,
						name: "x",
					},
				},
			},
		},
		{
			name: "call5",
			src:  "five(a, b; c, d; e)",
			n: &node{
				kind: nodeCall,
				name: "five",
				right: &node{
					kind: nodeArg,
					name: "",
					left: &node{
						kind: nodeName,
						name: "a",
					},
					right: &node{
						kind: nodeArg,
						name: ",",
						left: &node{
							kind: nodeName,
							name: "b",
						},
						right: &node{
							kind: nodeArg,
							name: ";",
							left: &node{
								kind: nodeName,
								name: "c",
							},
							right: &node{
								kind: nodeArg,
								name: ",",
								left: &node{
									kind: nodeName,
									name: "d",
								},
								right: &node{
									kind: nodeArg,
									name: ";",
									left: &node{
										kind: nodeName,
										name: "e",
									},
								},
							},
						},
					},
				},
			},
		},
	}
	ctx := NewContext(nil, testfns, 64)
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			a, err := Parse(strings.NewReader(c.src), ctx)
			if err != nil {
				t.Fatalf("%q failed to parse: %v", c.src, err)
			}
			d, e := a.n.diff(c.n)
			if d != nil || e != nil {
				t.Errorf("mismatched AST:\n\twant %v which has %v\n\tgot  %v which has %v from %q", c.n, e, a.n, d, c.src)
			}
		})
	}
}

func TestExprString(t *testing.T) {
	cases := []struct {
		name string
		src  string
	}{
		{"paren", "(x)"},
		{"square", "[x]"},
		{"curly", "{x}"},
		{"multi", "([{{[((x))]}}])"},

		{"plus", "+x"},
		{"neg", "-x"},
		{"negnum", "-1"},
		{"add", "x+y"},
		{"sub", "x-y"},
		{"mul", "x*y"},
		{"div", "x/y"},
		{"pow", "x^y"},
		{"altmul", "x×y"},
		{"altdiv", "x÷y"},
		{"terms", "x y"},
		{"parenterms", "x(y)"},

		{"call0", "zero()"},
		{"call0-terms", "zero x"},
		{"call1-bare", "one x"},
		{"call1-terms", "one a b c * d"},
		{"call1-plus", "one + x"},
		{"call1-add", "one x + y"},
		{"call1-exp", "one x^y"},
		{"call5", "five(a; b; c; d; e)"},

		{"add4", "w+x+y+z"},
		{"sub4", "w-x-y-z"},
		{"mul4", "w*x*y*z"},
		{"div4", "w/x/y/z"},
		{"pow4", "w^x^y^z"},
		{"terms4", "w x y z"},

		{"negpow", "-1^n"},
		{"desc", "w^x*y+z"},
		{"asc", "w+x*y^z"},
		{"descasc", "w^x*y+z+a*b^c"},
		{"ascdesc", "w+x*y^z^a*b+c"},
		{"negneg", "--x"},
		{"negsub", "-x-x"},
		{"powparen", "x^y(z)"},
		{"powneg", "x^-1"},
		{"powterms", "x y^z"},
		{"pownegpow", "x^-y^-z"},
		{"pownegneg", "x^--y"},
	}
	ctx := NewContext(nil, testfns, 64)
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			a, err := Parse(strings.NewReader(c.src), ctx)
			if err != nil {
				t.Fatalf("%q failed to parse: %v", c.src, err)
			}
			s := a.String()
			b, err := Parse(strings.NewReader(s), ctx)
			if err != nil {
				t.Fatalf("%q -> %q failed to parse: %v", c.src, s, err)
			}
			d, e := a.n.diff(b.n)
			if d != nil || e != nil {
				t.Errorf("mismatched AST:\n\t%q parses %v has %v\n\t%q parses %v has %v", c.src, a.n, d, s, b.n, e)
			}
		})
	}
}

func TestParseErrors(t *testing.T) {
	cases := []struct {
		name string
		src  string
		err  InputError
		res  []string
		excl []string
	}{
		{"empty", "", new(EmptyExpressionError), []string{`(?i)\b(no|empty)\b.*\bexpression\b`}, []string{`(?i)\bend\b`}},
		{"emptyparen", "()", new(EmptyExpressionError), []string{`(?i)\b(no|empty)\b.*\bexpression\b`, `\)`}, nil},
		{"emptyterm", "x()", new(EmptyExpressionError), []string{`(?i)\b(no|empty)\b.*\bexpression\b`, `\)`}, nil},
		{"emptyoperand", "x*", new(EmptyExpressionError), []string{`(?i)\b(no|empty)\b.*\bexpression\b`, `(?i)\bend\b`}, nil},
		{"emptyunary", "x*-", new(EmptyExpressionError), []string{`(?i)\b(no|empty)\b.*\bexpression\b`, `(?i)\bend\b`}, nil},
		{"left", "(x", new(BracketError), []string{`(?i)\bbracket\b`, `\(`}, nil},
		{"right", "x)", new(BracketError), []string{`(?i)\bbracket\b`, `\)`}, nil},
		{"mismatch", "(x]", new(BracketError), []string{`(?i)\bbracket\b`, `\(`, `]`}, nil},
		{"mismatch-mul", "x*(y]", new(BracketError), []string{`(?i)\bbracket\b`, `\(`, `]`}, nil},
		{"mismatch-terms", "x(y]", new(BracketError), []string{`(?i)\bbracket\b`, `\(`, `]`}, nil},
		{"nonunary", "*x", new(OperatorError), []string{`(?i)\bunary\b`, `(?i)\bop`, `\*`}, nil},
		{"sep", "x, y", new(SeparatorError), []string{`","`}, nil},
		{"sepbrackets", "(x, y)", new(SeparatorError), []string{`","`}, nil},
		{"call0-mismatch", "zero(x]", new(BracketError), []string{`(?i)\bbracket\b`, `\(`, `]`}, nil},
		{"call1-0", "one()", new(CallError), []string{`(?i)\bcall\b`, `\bone\b`, `\b((?i)0|zero)\b`}, nil},
		{"call1-eof", "one", new(CallError), []string{`(?i)\bcall\b`, `\bone\b`, `\b((?i)0|zero)\b`}, nil},
		{"call1-pareneof", "one(", new(BracketError), []string{`(?i)\bbracket\b`, `\(`}, nil},
		{"call1-mismatch", "one(x]", new(BracketError), []string{`(?i)\bbracket\b`, `\(`, `]`}, nil},
		{"call1-2", "one(x, y)", new(CallError), []string{`(?i)\bcall\b`, `\bone\b`, `\b2\b`}, nil},
		{"call1-empty", "one(; x)", new(SeparatorError), []string{`";"`}, nil},
		{"call1-empty2", "one(x;)", new(EmptyExpressionError), []string{`(?i)\b(no|empty)\b.*\bexpression\b`, `\)`}, nil},
		{"call5-4", "five(a, b, c, d)", new(CallError), []string{`(?i)\bcall\b`, `\bfive\b`, `\b4\b`}, nil},
		{"call5-bare", "five x", new(CallError), []string{`(?i)\bcall\b`, `\bfive\b`, `\b((?i)1|one)\b`}, nil},
		{"call5-empty", "five(a,,,,b)", new(SeparatorError), []string{`","`}, nil},
		{"lexer", "2^exp(-$)", new(LexError), []string{`\$`}, nil},
	}
	ctx := NewContext(nil, testfns, 64)
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			a, err := Parse(strings.NewReader(c.src), ctx)
			if a != nil {
				t.Errorf("%q parsed non-nil to %v", c.src, a.n)
			}
			if reflect.TypeOf(err) != reflect.TypeOf(c.err) {
				t.Errorf("wrong error type from %q: want %T, got %T", c.src, c.err, err)
			}
			msg := err.Error()
			for _, re := range c.res {
				if !regexp.MustCompile(re).MatchString(msg) {
					t.Errorf("error message %q does not match %s", msg, re)
				}
			}
			for _, re := range c.excl {
				if regexp.MustCompile(re).MatchString(msg) {
					t.Errorf("error message %q matches %s", msg, re)
				}
			}
		})
	}
}