package expressions

import (
	"errors"
	"io"
	"strconv"
	"strings"
	"unicode"
)

type lexToken struct {
	text string
	kind tokenKind
	pos  int
}

func (t lexToken) String() string {
	return t.kind.String() + ":" + t.text + "@" + strconv.Itoa(t.pos)
}

type tokenKind int

const (
	tokenNone tokenKind = iota
	// tokenEOF indicates the end of the input.
	tokenEOF
	// tokenNum is an integer, real, or imaginary token.
	tokenNum
	// tokenIdent is a variable or function name.
	tokenIdent
	// tokenOp is an operator.
	tokenOp
	// tokenOpen is an open bracket, e.g. (.
	tokenOpen
	// tokenClose is a close bracket, e.g. ).
	tokenClose
	// tokenSep is a function arguments separator, either , or ;.
	tokenSep
)

//go:generate go mod edit -require=golang.org/x/tools@v0.1.0
//go:generate go mod download
//go:generate go run golang.org/x/tools/cmd/stringer -type=tokenKind -trimprefix=token
//go:generate go mod tidy

// Operators contains the runes which are considered to be operators.
const Operators = "+-*/^×÷"

// OpenBrackets and CloseBrackets contain the runes which group expressions.
// The parser checks that a bracket in byte position k in OpenBrackets is
// matched with the bracket in byte position k in ClosedBrackets.
const (
	OpenBrackets  = "([{"
	CloseBrackets = ")]}"
)

func byteidcs(s string) []string {
	v := make([]string, len(s))
	for i, r := range s {
		v[i] = string(r)
	}
	return v
}

var (
	operstrs      = byteidcs(Operators)
	openbrackets  = byteidcs(OpenBrackets)
	closebrackets = byteidcs(CloseBrackets)
)

type lexer struct {
	src  io.RuneScanner
	buf  strings.Builder
	rune int
	p    lexToken
	eof  bool
}

func lex(src io.RuneScanner) *lexer {
	return &lexer{
		src:  src,
		rune: 1,
	}
}

// push unreads a token so that it is the next token returned from next. Panics
// if there is already a pushed token.
func (l *lexer) push(tok lexToken) {
	if l.p.kind != tokenNone {
		panic("expressions: double push")
	}
	l.p = tok
}

// must scans the pushed token. Panics if there is no pushed token.
func (l *lexer) must() lexToken {
	tok := l.p
	if tok.kind == tokenNone {
		panic("expressions: no pushed token")
	}
	l.p = lexToken{}
	return tok
}

// readRune reads a rune from the src and updates the lexer's position info.
func (l *lexer) readRune() (r rune, err error) {
	r, sz, err := l.src.ReadRune()
	if sz > 0 {
		l.rune++
	}
	return r, err
}

// unreadeRune unreads a rune from the src and updates the lexer's position
// info. Panics if unreading returns an error.
func (l *lexer) unreadRune() {
	if err := l.src.UnreadRune(); err != nil {
		panic(err)
	}
	l.rune--
}

// next scans the next token from the input. The first time EOF is encountered
// before any non-whitespace characters, the result is an EOF token with a nil
// error. Subsequent times, if the EOF token is not pushed, the result is an
// empty token with io.EOF.
func (l *lexer) next(wseof string) (lexToken, error) {
	if l.p.kind != tokenNone {
		tok := l.p
		l.p = lexToken{}
		return tok, nil
	}
	if l.eof {
		return lexToken{}, io.EOF
	}
	defer l.buf.Reset()
	tok := lexToken{pos: l.rune}
	for {
		r, err := l.readRune()
		if err != nil {
			if errors.Is(err, io.EOF) {
				tok.kind = tokenEOF
				l.eof = true
				return tok, nil
			}
			// TODO(zeph): wrap?
			return tok, err
		}
		switch {
		case unicode.IsSpace(r):
			if strings.ContainsRune(wseof, r) {
				tok.kind = tokenEOF
				l.eof = true
				return tok, nil
			}
			tok.pos++
			continue
		case '0' <= r && r <= '9', r == '.':
			l.unreadRune()
			if err := l.scanNum(); err != nil {
				return tok, err
			}
			tok.text = l.buf.String()
			tok.kind = tokenNum
			return tok, nil
		case r == '_', unicode.IsLetter(r):
			l.unreadRune()
			if err := l.scanIdent(); err != nil {
				return tok, err
			}
			tok.text = l.buf.String()
			// inf looks like an identifier, so check for it here.
			switch tok.text {
			case "inf", "Inf":
				tok.kind = tokenNum
			default:
				tok.kind = tokenIdent
			}
			return tok, nil
		case r == ',':
			tok.text = ","
			tok.kind = tokenSep
			return tok, nil
		case r == ';':
			tok.text = ";"
			tok.kind = tokenSep
			return tok, nil
		case r == '∞':
			tok.text = "∞"
			tok.kind = tokenNum
			return tok, nil
		default:
			if k := strings.IndexRune(Operators, r); k >= 0 {
				tok.text = operstrs[k]
				tok.kind = tokenOp
				return tok, nil
			}
			if k := strings.IndexRune(OpenBrackets, r); k >= 0 {
				tok.text = openbrackets[k]
				tok.kind = tokenOpen
				return tok, nil
			}
			if k := strings.IndexRune(CloseBrackets, r); k >= 0 {
				tok.text = closebrackets[k]
				tok.kind = tokenClose
				return tok, nil
			}
			// Write the rune so that it shows up in the error message.
			l.buf.WriteRune(r)
			return tok, l.error("")
		}
	}
}

func (l *lexer) scanNum() error {
	var dig, dot, e, le, ed bool
	for {
		r, err := l.readRune()
		if err != nil {
			if errors.Is(err, io.EOF) {
				break
			}
			return err
		}
		if unicode.IsSpace(r) {
			l.unreadRune()
			break
		}
		if r == '+' || r == '-' {
			// + or - anywhere other than immediately following an exponent
			// marker means a new token, as it is an operator.
			if !le {
				l.unreadRune()
				break
			}
			le = false
			l.buf.WriteRune(r)
			continue
		}
		if strings.ContainsRune(Operators+OpenBrackets+CloseBrackets+",;", r) {
			l.unreadRune()
			break
		}
		l.buf.WriteRune(r)
		switch r {
		case '.':
			if dot || e {
				return l.error("number")
			}
			dot = true
			le = false
		case 'e', 'E':
			if !dig || e {
				return l.error("number")
			}
			e = true
			le = true
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			if e {
				ed = true
			} else {
				dig = true
			}
			le = false
		default:
			return l.error("number")
		}
	}
	if (!dig && !ed) || (e && !ed) {
		return l.error("number")
	}
	return nil
}

func (l *lexer) scanIdent() error {
	for {
		r, err := l.readRune()
		if err != nil {
			if errors.Is(err, io.EOF) {
				// next unreads the rune that decides ident scanning before
				// calling scanIdent, so we have scanned at least one rune.
				return nil
			}
			return err
		}
		switch {
		case r == '_', r == '.', unicode.IsLetter(r), unicode.IsDigit(r):
			l.buf.WriteRune(r)
		default:
			l.unreadRune()
			return nil
		}
	}
}

func (l *lexer) error(kind string) error {
	return &LexError{
		Text: l.buf.String(),
		Kind: kind,
		Col:  l.rune,
	}
}

// LexError indicates an invalid token. It implements InputError.
type LexError struct {
	// Text is the token the lexer was scanning when the invalid rune was
	// encountered, plus the invalid rune.
	Text string
	// Kind is the type of token the lexer was scanning. This may be "number",
	// "identifier", "operator", or the empty string (if a token kind hadn't
	// been decided).
	Kind string
	// Col is the total number of runes scanned by the lexer up to and
	// including this error.
	Col int
}

func (err *LexError) Error() string {
	pos := "column " + strconv.Itoa(err.Col)
	if err.Kind == "" {
		return "invalid token at " + pos + ": " + err.Text
	}
	return "invalid " + err.Kind + " token at " + pos + ": " + err.Text
}

func (err *LexError) Pos() int {
	return err.Col
}
