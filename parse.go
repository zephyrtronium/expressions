package expressions

import (
	"io"
	"math/big"
	"sort"
	"strconv"
	"strings"
	"unicode/utf8"
)

// Expr = num | name | Call | Neg | Plus | Add | Sub | Mul | Div | Pow | '(' Expr ')' | '[' Expr ']' | '{' Expr '}'
// Call = funcname | funcname Expr | funcname ArgList
// ArgList = '(' Expr { (',' | ';') Expr } ')' | '[' Expr { (',' | ';') Expr } ']' | '{' Expr { (',' | ';') Expr } '}'
// Neg = '-' Expr
// Plus = '+' Expr
// Add = Expr '+' Expr
// Sub = Expr '-' Expr
// Mul = Expr '*' Expr | Expr '×' Expr
// Div = Expr '/' Expr | Expr '÷' Expr
// Pow = Expr '^' Expr

// Expr is a parsed expression that can be evaluated with a context.
type Expr struct {
	// n is the root node of the expression.
	n *node
	// names is the list of variable names used in the expression.
	names []string
}

// Parse parses an expression so it can be evaluated with a context. ctx
// supplies precision and function names for parsing only; its variable names
// are ignored. If ctx is nil, a default context is used containing the same
// functions as the default for NewContext.
func Parse(src io.RuneScanner, ctx *Context) (*Expr, error) {
	if ctx == nil {
		ctx = &defaultctx
	}
	scan := lex(src)
	p := parsectx{
		names: make(map[string]bool),
		funcs: ctx.funcs,
		prec:  ctx.prec,
	}
	n, err := parseterm(scan, &p, exprprec)
	if err != nil {
		return nil, err
	}
	if tok := scan.must(); tok.kind != tokenEOF {
		return nil, itShouldNotHaveEndedThisWay(tok, -1)
	}
	ex := Expr{
		n:     n,
		names: make([]string, 0, len(p.names)),
	}
	for k := range p.names {
		ex.names = append(ex.names, k)
	}
	sort.Strings(ex.names)
	return &ex, nil
}

// parsectx holds general data for parsing.
type parsectx struct {
	// names is the set of variable names that have been seen this parse.
	names map[string]bool
	// funcs is the set of function names that trigger special parsing for ids.
	funcs map[string]Func
	// resv is a reserved parsed node. parsearglist sets this when it parses a
	// single parenthesized term so that the parser can back it out to an
	// implicit multiplication if the function is niladic.
	resv *node
	// prec is the precision to which to parse numbers.
	prec uint
}

// parseterm parses a single term. If there is no error, then parseterm pushes
// the last token it scans, including EOF. If the input is an empty
// subexpression, the result is nil with no error; callers must create an error
// in contexts where empty subexpressions are illegal.
func parseterm(scan *lexer, p *parsectx, until operator) (*node, error) {
	n, err := parselhs(scan, p, until)
	if err != nil {
		return nil, err
	}
	if n == nil {
		return nil, nil
	}
	if p.resv != nil {
		// parselhs parsed a niladic function followed by a parenthesized term.
		// So, the parsing here is as if we encountered an open bracket, except
		// that the contents are already parsed and valid.
		prec := termprec
		if !prec.moreBinding(until) {
			return n, nil
		}
		n = &node{kind: nodeMul, left: n, right: p.resv}
		p.resv = nil
	}
	for {
		tok, err := scan.next()
		if err != nil {
			return nil, err
		}
		switch tok.kind {
		case tokenNum, tokenIdent:
			// (parsed) x -> (parsed) * (x)
			// (parsed) x^(expr) -> (parsed) * (x^(expr))
			// a^(parsed) x -> (a^(parsed)) * (x)
			scan.push(tok)
			prec := termprec
			if !prec.moreBinding(until) {
				return n, nil
			}
			rhs, err := parseterm(scan, p, prec)
			if err != nil {
				return nil, err
			}
			n = &node{kind: nodeMul, left: n, right: rhs}
		case tokenOp:
			// Binary operator.
			prec, ok := binops[tok.text]
			if !ok {
				return nil, &OperatorError{Col: tok.pos, Operator: tok.text, Unary: false}
			}
			if !prec.moreBinding(until) {
				scan.push(tok)
				return n, nil
			}
			rhs, err := parseterm(scan, p, prec)
			if err != nil {
				return nil, err
			}
			n = &node{kind: prec.op, left: n, right: rhs}
		case tokenOpen:
			// Since parselhs parses functions aggressively, this is a
			// multiplication by a parenthesized term: 2 (expr) -> (2) * (expr).
			match := rightbracket(tok.text)
			prec := termprec
			if !prec.moreBinding(until) {
				scan.push(tok)
				return n, nil
			}
			rhs, err := parseterm(scan, p, prec)
			if err != nil {
				return nil, err
			}
			end := scan.must()
			if end.kind != tokenClose || end.text != closebrackets[match] {
				return nil, itShouldNotHaveEndedThisWay(end, match)
			}
			if rhs == nil {
				return nil, &EmptyExpressionError{Col: end.pos, End: end.text}
			}
			n = &node{kind: nodeMul, left: n, right: rhs}
		case tokenClose, tokenSep, tokenEOF:
			// End of expression.
			scan.push(tok)
			return n, nil
		default:
			panic("expressions: unknown token: " + tok.String())
		}
	}
}

// parselhs parses the first component of a term. I.e., operators are unary,
// and any encountered token must be valid as the start of a subexpression.
func parselhs(scan *lexer, p *parsectx, until operator) (*node, error) {
	tok, err := scan.next()
	if err != nil {
		return nil, err
	}
	var n *node
	switch tok.kind {
	case tokenNum:
		n = &node{kind: nodeNum, num: new(big.Float).SetPrec(p.prec)}
		if _, ok := n.num.SetString(tok.text); !ok {
			panic("invalid number text: " + tok.text)
		}
	case tokenIdent:
		fn := p.funcs[tok.text]
		if fn == nil {
			p.names[tok.text] = true
			n = &node{kind: nodeName, name: tok.text}
		} else {
			rhs, err := parsecall(scan, p, until, fn, tok.text)
			if err != nil {
				return nil, err
			}
			// If fn is niladic and the call is like fn(a), then the result
			// from parsecall is nil, nil, and p.resv is non-nil.
			n = &node{kind: nodeCall, name: tok.text, right: rhs}
		}
	case tokenOp:
		// unary operator
		prec, ok := preops[tok.text]
		if !ok {
			return nil, &OperatorError{Col: tok.pos, Operator: tok.text, Unary: true}
		}
		if !prec.moreBinding(until) {
			// x^-y -> x^(-y)
			// Just use the new operator's precedence to simplify.
			prec.prec, prec.right = until.prec, until.right
		}
		rhs, err := parseterm(scan, p, prec)
		if err != nil {
			return nil, err
		}
		n = &node{kind: prec.op, left: rhs}
	case tokenOpen:
		match := rightbracket(tok.text)
		rhs, err := parseterm(scan, p, exprprec)
		if err != nil {
			return nil, err
		}
		end := scan.must()
		if end.kind != tokenClose || end.text != closebrackets[match] {
			return nil, itShouldNotHaveEndedThisWay(end, match)
		}
		if rhs == nil {
			return nil, &EmptyExpressionError{Col: end.pos, End: end.text}
		}
		n = rhs
	case tokenClose:
		// This might be part of niladic func(), so just let the caller decide
		// what to do.
		scan.push(tok)
		return nil, nil
	case tokenSep:
		return nil, &SeparatorError{Col: tok.pos, Sep: tok.text}
	case tokenEOF:
		return nil, &EmptyExpressionError{Col: tok.pos, End: ""}
	default:
		panic("expressions: unknown token: " + tok.String())
	}
	return n, nil
}

// parsecall parses the arguments to a call of a given Func.
func parsecall(scan *lexer, p *parsectx, until operator, fn Func, name string) (*node, error) {
	tok, err := scan.next()
	if err != nil {
		return nil, err
	}
	switch tok.kind {
	case tokenNum, tokenIdent, tokenOp:
		switch {
		case fn.CanCall(1):
			// Single argument. exp x -> exp(x)
			scan.push(tok)
			if termprec.moreBinding(until) {
				until = termprec
			}
			rhs, err := parseterm(scan, p, until)
			if err != nil {
				return nil, err
			}
			return &node{kind: nodeArg, left: rhs}, nil
		case fn.CanCall(0):
			// No argument. pi x -> (pi) * (x)
			scan.push(tok)
		default:
			// Any other number of arguments requires brackets.
			return nil, &CallError{Col: tok.pos, Func: name, Len: 1}
		}
	case tokenOpen:
		match := rightbracket(tok.text)
		n, len, err := parsearglist(scan, p, tok.text)
		if err != nil {
			return nil, err
		}
		end := scan.must()
		if end.kind != tokenClose {
			panic("expressions: parsearglist ended on " + end.String() + " instead of close bracket")
		}
		if end.text != closebrackets[match] {
			return nil, &BracketError{Col: end.pos, Left: tok.text, Right: end.text}
		}
		if !fn.CanCall(len) {
			if p.resv != nil && fn.CanCall(0) {
				// If fn is niladic, convert from fn(a) to fn()*a.
				return nil, nil
			}
			p.resv = nil
			return nil, &CallError{Col: tok.pos, Func: name, Len: len}
		}
		p.resv = nil
		return n, nil
	case tokenClose, tokenSep, tokenEOF:
		if !fn.CanCall(0) {
			return nil, &CallError{Col: tok.pos, Func: name}
		}
		scan.push(tok)
	default:
		panic("expressions: unknown token: " + tok.String())
	}
	return nil, nil
}

// parsearglist parses a bracketed list of zero or more args.
func parsearglist(scan *lexer, p *parsectx, open string) (*node, int, error) {
	var n node
	l := &n
	len := 0
	pb := ""
	for {
		rhs, err := parseterm(scan, p, exprprec)
		if err != nil {
			// As a special case, reporting mismatched brackets is more helpful
			// than empty expression, if that's what we'd do here.
			if ee, _ := err.(*EmptyExpressionError); ee != nil {
				err = &BracketError{Col: ee.Col, Left: open}
			}
			return nil, 0, err
		}
		end := scan.must()
		switch end.kind {
		case tokenClose:
			// Caller checks that brackets match.
			scan.push(end)
			if rhs == nil {
				// No expression parsed.
				// func() is allowed, but func(a,) isn't.
				// TODO(zeph): allow func(a,)
				if len != 0 {
					return nil, 0, &EmptyExpressionError{Col: end.pos, End: end.text}
				}
				return nil, 0, nil
			}
			l.right = &node{kind: nodeArg, name: pb, left: rhs}
			if len == 0 {
				// func(a). If func is niladic, then this is an implicit
				// multiplication. Reserve the rhs so that the parser can
				// convert from a function call.
				p.resv = rhs
			}
			return n.right, len + 1, nil
		case tokenSep:
			// TODO(zeph): allow e.g. hyper(; ; x) for 0F0;
			// currently would need to check for separator error from parseterm
			len++
			l.right = &node{kind: nodeArg, name: pb, left: rhs}
			l = l.right
			pb = end.text
		case tokenEOF:
			return nil, 0, &BracketError{Col: end.pos, Left: open, Right: ""}
		default:
			panic("expressions: parseexpr ended on non-end token " + end.String())
		}
	}
}

// rightbracket gets the closing bracket index for an opening bracket.
func rightbracket(left string) int {
	r, sz := utf8.DecodeRuneInString(left)
	k := strings.IndexRune(OpenBrackets, r)
	if k < 0 || sz != len(left) {
		panic("expressions: invalid bracket " + strconv.Quote(left))
	}
	return k
}

// leftbracket gets the opening bracket matching right. If right is no bracket,
// then the result is the empty string.
func leftbracket(right int) string {
	if right == -1 {
		return ""
	}
	return openbrackets[right]
}

// itShouldNotHaveEndedThisWay returns an error appropriate for an unexpected
// token at the end of a subexpression. match is the bracket rune index that
// the expression should have matched, or -1 if none.
func itShouldNotHaveEndedThisWay(tok lexToken, match int) error {
	switch tok.kind {
	case tokenEOF:
		// Unexpected EOF implies an open bracket that was not closed.
		return &BracketError{Col: tok.pos, Left: leftbracket(match), Right: ""}
	case tokenClose:
		// A bracket could be the wrong bracket for the opening brace or any
		// bracket at the end of an input.
		return &BracketError{Col: tok.pos, Left: leftbracket(match), Right: tok.text}
	case tokenSep:
		// Separator outside a function call.
		return &SeparatorError{Col: tok.pos, Sep: tok.text}
	default:
		panic("expressions: it really should not have ended this way: " + tok.String())
	}
}

// Eval evaluates the expression using a context and returns the result. If an
// error occurs, e.g. a missing variable definition or an argument to a
// function is outside the function's domain, then the result is nil and
// ctx.Err returns the error.
func (e *Expr) Eval(ctx *Context) *big.Float {
	if err := e.n.eval(ctx); err != nil {
		ctx.err = err
		return nil
	}
	return ctx.Result()
}

// Vars returns the variable names used when evaluating the expression.
func (e *Expr) Vars() []string {
	return append(([]string)(nil), e.names...)
}

// String creates a string representation of the parsed expression, with
// alternating round and square brackets grouping each term.
func (e *Expr) String() string {
	var b strings.Builder
	e.n.fmt(&b, false, true)
	return b.String()
}

type operator struct {
	// prec is the precedence value. Lower is more binding.
	prec int8
	// right indicates right-associativity.
	right bool
	// op is the node kind to use when this operator is selected.
	op nodeKind
}

func (p operator) moreBinding(than operator) bool {
	if p.prec != than.prec {
		return p.prec > than.prec
	}
	return p.right
}

var (
	// binops is the binary operators.
	binops = map[string]operator{
		"+": {1, false, nodeAdd},
		"-": {1, false, nodeSub},
		"*": {5, false, nodeMul},
		"/": {5, false, nodeDiv},
		"^": {15, true, nodePow},
		"×": {5, false, nodeMul},
		"÷": {5, false, nodeDiv},
	}
	// preops is the unary operators.
	preops = map[string]operator{
		"+": {10, true, nodeNop},
		"-": {10, true, nodeNeg},
	}
	// termprec is the default precedence for parsing terms. Its prec
	// should match that of multiplication.
	termprec = operator{5, true, nodeMul}
	// exprprec is the precedence required to parse an entire subexpression.
	exprprec = operator{-128, true, nodeNone}
)
