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
	// tokenNum is an integer, real, or imaginary token.
	tokenNum
	// tokenIdent is a variable or operator.
	tokenIdent
	// tokenBracket is a bracket, i.e. ().
	tokenBracket
)

//go:generate go run golang.org/x/tools/cmd/stringer -type=tokenKind -trimprefix=token

// Operators contains the runes which are considered to be operators.
const Operators = "+-*/^×÷"

// Brackets contains the runes which are considered to group expressions.
// The parser checks that a bracket in position 2k+1 in this string is matched
// with the bracket in position 2k, where k starts at 0.
const Brackets = "()[]{}"

type lexer struct {
	src  io.RuneScanner
	buf  strings.Builder
	rune int
	byte int
	lsz  int
}

func lex(src io.RuneScanner) *lexer {
	return &lexer{
		src:  src,
		rune: 1,
		byte: 1,
	}
}

// readRune reads a rune from the src and updates the lexer's position info.
func (l *lexer) readRune() (r rune, err error) {
	r, sz, err := l.src.ReadRune()
	l.lsz = sz
	if sz > 0 {
		l.rune++
		l.byte += sz
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
	l.byte -= l.lsz
}

func (l *lexer) next() (lexToken, error) {
	l.buf.Reset()
	tok := lexToken{pos: l.rune}
	for {
		r, err := l.readRune()
		if err != nil {
			// TODO: wrap?
			return tok, err
		}
		switch {
		case unicode.IsSpace(r):
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
			tok.kind = tokenIdent
			return tok, nil
		case strings.ContainsRune(Operators, r):
			tok.text = string(r)
			tok.kind = tokenIdent
			return tok, nil
		case strings.ContainsRune(Brackets, r):
			tok.text = string(r)
			tok.kind = tokenBracket
			return tok, nil
		default:
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
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			return err
		}
		if unicode.IsSpace(r) {
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
		if strings.ContainsRune(Operators, r) || strings.ContainsRune(Brackets, r) {
			l.unreadRune()
			break
		}
		l.buf.WriteRune(r)
		switch r {
		case '.':
			if dot {
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
		if errors.Is(err, io.EOF) {
			// next unreads the rune that decides ident scanning before calling
			// scanIdent, so we have scanned at least one rune.
			return nil
		}
		if err != nil {
			return err
		}
		switch {
		case r == '_', r == '.', unicode.IsLetter(r), unicode.IsDigit(r):
			l.buf.WriteRune(r)
		case unicode.IsSpace(r):
			return nil
		default:
			l.unreadRune()
			return nil
		}
	}
}

func (l *lexer) reset(src io.RuneScanner) {
	l.src = src
	l.rune = 0
}

func (l *lexer) error(kind string) error {
	return &LexError{
		Text: l.buf.String(),
		Kind: kind,
		Col:  l.rune,
		Byte: l.byte,
	}
}

// LexError indicates an invalid token.
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
	// Byte is the total number of bytes scanned by the lexer.
	Byte int
}

func (err *LexError) Error() string {
	pos := "column " + strconv.Itoa(err.Col)
	if err.Col != err.Byte {
		pos += " (byte " + strconv.Itoa(err.Byte) + ")"
	}
	if err.Kind == "" {
		return "invalid token at " + pos + ": " + err.Text
	}
	return "invalid " + err.Kind + " token at " + pos + ": " + err.Text
}
