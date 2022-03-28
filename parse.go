package expressions

import (
	"io"
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

// Parse parses an expression so it can be evaluated with a context. The given
// options are applied in order.
func Parse(src io.RuneScanner, opts ...ParseOption) (*Expr, error) {
	scan := lex(src)
	p := parsectx{
		names: make(map[string]bool),
	}
	for _, opt := range opts {
		p = opt.parseOption(p)
	}
	if p.funcs == nil {
		p.funcs = globalfuncs
	} else if !p.nodefaults {
		// Only set default functions that aren't already set.
		for k, v := range globalfuncs {
			if _, ok := p.funcs[k]; !ok {
				p.funcs[k] = v
			}
		}
	}
	n, err := parseterm(scan, &p, exprprec)
	if err != nil {
		return nil, err
	}
	switch tok := scan.must(); tok.kind {
	case tokenEOF:
	case tokenSep:
		switch {
		case p.ceof && tok.text == ",":
		case p.seof && tok.text == ";":
		default:
			return nil, itShouldNotHaveEndedThisWay(tok, -1)
		}
	default:
		return nil, itShouldNotHaveEndedThisWay(tok, -1)
	}
	ex := Expr{
		n:     n,
		names: make([]string, 0, len(p.names)),
	}
	for k := range p.names {
		ex.names = append(ex.names, k)
	}
	sortstrs(ex.names)
	return &ex, nil
}

// sortstrs sorts a string slice without using package sort because that has
// reflection and allocation problems.
func sortstrs(names []string) {
	for i := 1; i < len(names); i++ {
		for j := i; j > 0 && names[j] < names[j-1]; j-- {
			names[j], names[j-1] = names[j-1], names[j]
		}
	}
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
		r := p.resv
		p.resv = nil
		r, err := parseonto(scan, p, prec, r)
		if err != nil {
			return nil, err
		}
		n = &node{kind: nodeMul, left: n, right: r}
	}
	return parseonto(scan, p, until, n)
}

// parseonto parses assuming that n is the lhs of the expression.
func parseonto(scan *lexer, p *parsectx, until operator, n *node) (*node, error) {
	for {
		tok, err := scan.next(p.wseof)
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
			prec := binop(tok.text)
			if prec.op == nodeNone {
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
			if rhs == nil {
				return nil, &EmptyExpressionError{Col: tok.pos, End: scan.must().text}
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
// any encountered token must be valid as the start of a subexpression, and
// whitespace normally lexed as EOF is ignored.
func parselhs(scan *lexer, p *parsectx, until operator) (*node, error) {
	// Don't use EOF whitespace for LHS.
	tok, err := scan.next("")
	if err != nil {
		return nil, err
	}
	var n *node
	switch tok.kind {
	case tokenNum:
		n = &node{kind: nodeNum, name: tok.text}
	case tokenIdent:
		fn := p.funcs[tok.text]
		if fn == nil {
			p.names[tok.text] = true
			n = &node{kind: nodeName, name: tok.text}
		} else {
			rhs, exp, err := parsecall(scan, p, until, fn, tok.text)
			if err != nil {
				return nil, err
			}
			// If fn is niladic and the call is like fn(a), then the result
			// from parsecall is nil, nil, and p.resv is non-nil.
			n = &node{kind: nodeCall, name: tok.text, fn: fn, right: rhs}
			if exp != nil {
				exp.left = n
				n = exp
			}
		}
	case tokenOp:
		// unary operator
		prec := unop(tok.text)
		if prec.op == nodeNone {
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
		if rhs == nil {
			return nil, &EmptyExpressionError{Col: tok.pos, End: scan.must().text}
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
		switch tok.text {
		case ",":
			if p.ceof {
				scan.push(tok)
				return nil, nil
			}
		case ";":
			if p.seof {
				scan.push(tok)
				return nil, nil
			}
		default:
			panic("expressions: invalid separator " + strconv.Quote(tok.text))
		}
		return nil, &SeparatorError{Col: tok.pos, Sep: tok.text}
	case tokenEOF:
		return nil, &EmptyExpressionError{Col: tok.pos, End: ""}
	default:
		panic("expressions: unknown token: " + tok.String())
	}
	return n, nil
}

// parsecall parses the arguments to a call of a given Func. The second result,
// if non-nil, is a node that the function call is lhs to.
func parsecall(scan *lexer, p *parsectx, until operator, fn Func, name string) (*node, *node, error) {
	// We respect whitespace here so that pi\nx doesn't string
	// together expressions.
	tok, err := scan.next(p.wseof)
	if err != nil {
		return nil, nil, err
	}
	switch tok.kind {
	case tokenOp:
		// Check for e.g. ^2 in cos^2 x. Must be an exponentiation or higher.
		// Note that the fact that exponentiation is important here:
		// func^x^y(z) parses as [func(z)]^(x^y).
		if prec := binop(tok.text); prec.moreBinding(powprec) {
			up, err := parseterm(scan, p, powprec)
			if err != nil {
				return nil, nil, err
			}
			if p.resv != nil {
				// The exponentiated term was itself a niladic function with a
				// parenthesized expression following it, like fn^pi(x), where
				// p.resv is (x). If we can call fn with one argument, then x
				// is it; if zero, then x is an implicit multiplication.
				switch {
				case fn.CanCall(1):
					args := &node{kind: nodeArg, left: p.resv}
					exp := &node{kind: nodePow, right: up}
					p.resv = nil
					return args, exp, nil
				case fn.CanCall(0):
					exp := &node{kind: nodePow, right: up}
					// Leave resv to the caller.
					return nil, exp, nil
				default:
					return nil, nil, &CallError{Col: tok.pos, Func: name, Len: 1}
				}
			}
			args, ee, err := parsecall(scan, p, until, fn, name)
			if err != nil {
				return nil, nil, err
			}
			if ee != nil {
				// The precedence we parsed is right-associative and higher
				// than any other. With the current rules, there should never
				// be an additional exponent here.
				panic("expressions: parsed second call exponent: " + ee.String())
			}
			// The caller fills in up.left.
			exp := &node{kind: nodePow, right: up}
			return args, exp, nil
		}
		// Other than exponentiations, finding an operator is the same as
		// finding a number or identifier.
		fallthrough
	case tokenNum, tokenIdent:
		switch {
		case fn.CanCall(1):
			// Single argument. exp x -> exp(x)
			scan.push(tok)
			if termprec.moreBinding(until) {
				until = termprec
			}
			rhs, err := parseterm(scan, p, until)
			if err != nil {
				return nil, nil, err
			}
			return &node{kind: nodeArg, left: rhs}, nil, nil
		case fn.CanCall(0):
			// No argument. pi x -> (pi) * (x)
			scan.push(tok)
		default:
			// Any other number of arguments requires brackets.
			return nil, nil, &CallError{Col: tok.pos, Func: name, Len: 1}
		}
	case tokenOpen:
		match := rightbracket(tok.text)
		n, len, err := parsearglist(scan, p, tok.text)
		if err != nil {
			return nil, nil, err
		}
		end := scan.must()
		if end.kind != tokenClose {
			panic("expressions: parsearglist ended on " + end.String() + " instead of close bracket")
		}
		if end.text != closebrackets[match] {
			return nil, nil, &BracketError{Col: end.pos, Left: tok.text, Right: end.text}
		}
		if !fn.CanCall(len) {
			if p.resv != nil && fn.CanCall(0) {
				// If fn is niladic, convert from fn(a) to fn()*a.
				return nil, nil, nil
			}
			p.resv = nil
			return nil, nil, &CallError{Col: tok.pos, Func: name, Len: len}
		}
		p.resv = nil
		return n, nil, nil
	case tokenClose, tokenSep, tokenEOF:
		if !fn.CanCall(0) {
			return nil, nil, &CallError{Col: tok.pos, Func: name}
		}
		scan.push(tok)
	default:
		panic("expressions: unknown token: " + tok.String())
	}
	return nil, nil, nil
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

// binop gets a binary operator for a token string. If there is no such binary
// operator, then the result has an op of nodeNone.
func binop(text string) operator {
	switch text {
	case "+":
		return operator{1, false, nodeAdd}
	case "-":
		return operator{1, false, nodeSub}
	case "*":
		return operator{5, false, nodeMul}
	case "/":
		return operator{5, false, nodeDiv}
	case "^":
		return operator{15, true, nodePow}
	case "×":
		return operator{5, false, nodeMul}
	case "÷":
		return operator{5, false, nodeDiv}
	default:
		return operator{}
	}
}

// unop gets a unary operator for a token string. If there is no such unary
// operator, then the result has an op of nodeNone.
func unop(text string) operator {
	switch text {
	case "+":
		return operator{10, true, nodeNop}
	case "-":
		return operator{10, true, nodeNeg}
	default:
		return operator{}
	}
}

var (
	// termprec is the default precedence for parsing terms. Its prec
	// should match that of multiplication.
	termprec = operator{5, true, nodeMul}
	// powprec is the precedence of exponentiation.
	powprec = binop("^")
	// exprprec is the precedence required to parse an entire subexpression.
	exprprec = operator{-128, true, nodeNone}
)
