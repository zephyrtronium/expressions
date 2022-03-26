package expressions

import (
	"io"
	"strings"
	"testing"
)

func TestLex(t *testing.T) {
	cases := []struct {
		src    string
		tokens []lexToken
		errs   int
	}{
		// spaces
		{"", []lexToken{{kind: tokenEOF, pos: 1}}, 0},
		{" \t \r\n ", []lexToken{{kind: tokenEOF, pos: 7}}, 0},
		// numbers
		{"0", []lexToken{{text: "0", kind: tokenNum, pos: 1}, {kind: tokenEOF, pos: 2}}, 0},
		{"9876543210", []lexToken{{text: "9876543210", kind: tokenNum, pos: 1}, {kind: tokenEOF, pos: 11}}, 0},
		{"1 0", []lexToken{{text: "1", kind: tokenNum, pos: 1}, {text: "0", kind: tokenNum, pos: 3}, {kind: tokenEOF, pos: 4}}, 0},
		{"1.0", []lexToken{{text: "1.0", kind: tokenNum, pos: 1}, {kind: tokenEOF, pos: 4}}, 0},
		{"-1", []lexToken{{text: "-", kind: tokenOp, pos: 1}, {text: "1", kind: tokenNum, pos: 2}, {kind: tokenEOF, pos: 3}}, 0},
		{"1e1", []lexToken{{text: "1e1", kind: tokenNum, pos: 1}, {kind: tokenEOF, pos: 4}}, 0},
		{"1e", []lexToken{{pos: 1}, {kind: tokenEOF, pos: 3}}, 1},
		{"1e1e1", []lexToken{{pos: 1}, {kind: tokenNum, text: "1", pos: 5}, {kind: tokenEOF, pos: 6}}, 1},
		{"1e+1", []lexToken{{text: "1e+1", kind: tokenNum, pos: 1}, {kind: tokenEOF, pos: 5}}, 0},
		{"1e-1", []lexToken{{text: "1e-1", kind: tokenNum, pos: 1}, {kind: tokenEOF, pos: 5}}, 0},
		{"1.1.1", []lexToken{{pos: 1}, {text: "1", kind: tokenNum, pos: 5}, {kind: tokenEOF, pos: 6}}, 1},
		{"1.0e1", []lexToken{{text: "1.0e1", kind: tokenNum, pos: 1}, {kind: tokenEOF, pos: 6}}, 0},
		{".", []lexToken{{pos: 1}, {kind: tokenEOF, pos: 2}}, 1},
		{".1", []lexToken{{text: ".1", kind: tokenNum, pos: 1}, {kind: tokenEOF, pos: 3}}, 0},
		{".1e1", []lexToken{{text: ".1e1", kind: tokenNum, pos: 1}, {kind: tokenEOF, pos: 5}}, 0},
		{"1+0", []lexToken{{text: "1", kind: tokenNum, pos: 1}, {text: "+", kind: tokenOp, pos: 2}, {text: "0", kind: tokenNum, pos: 3}, {kind: tokenEOF, pos: 4}}, 0},
		{"1*0", []lexToken{{text: "1", kind: tokenNum, pos: 1}, {text: "*", kind: tokenOp, pos: 2}, {text: "0", kind: tokenNum, pos: 3}, {kind: tokenEOF, pos: 4}}, 0},
		{"(1)", []lexToken{{text: "(", kind: tokenOpen, pos: 1}, {text: "1", kind: tokenNum, pos: 2}, {text: ")", kind: tokenClose, pos: 3}, {kind: tokenEOF, pos: 4}}, 0},
		{"1a", []lexToken{{pos: 1}, {kind: tokenEOF, pos: 3}}, 1},
		{"inf", []lexToken{{text: "inf", kind: tokenNum, pos: 1}, {kind: tokenEOF, pos: 4}}, 0},
		{"Inf", []lexToken{{text: "Inf", kind: tokenNum, pos: 1}, {kind: tokenEOF, pos: 4}}, 0},
		{"∞", []lexToken{{text: "∞", kind: tokenNum, pos: 1}, {kind: tokenEOF, pos: 2}}, 0},
		// identifiers
		{"e", []lexToken{{text: "e", kind: tokenIdent, pos: 1}, {kind: tokenEOF, pos: 2}}, 0},
		{"e1", []lexToken{{text: "e1", kind: tokenIdent, pos: 1}, {kind: tokenEOF, pos: 3}}, 0},
		{"π", []lexToken{{text: "π", kind: tokenIdent, pos: 1}, {kind: tokenEOF, pos: 2}}, 0},
		{"eπ", []lexToken{{text: "eπ", kind: tokenIdent, pos: 1}, {kind: tokenEOF, pos: 3}}, 0},
		{"INF", []lexToken{{text: "INF", kind: tokenIdent, pos: 1}, {kind: tokenEOF, pos: 4}}, 0},
		{"_1234_", []lexToken{{text: "_1234_", kind: tokenIdent, pos: 1}, {kind: tokenEOF, pos: 7}}, 0},
		{"e(", []lexToken{{text: "e", kind: tokenIdent, pos: 1}, {text: "(", kind: tokenOpen, pos: 2}, {kind: tokenEOF, pos: 3}}, 0},
		// operators
		{"+", []lexToken{{text: "+", kind: tokenOp, pos: 1}, {kind: tokenEOF, pos: 2}}, 0},
		{"++", []lexToken{{text: "+", kind: tokenOp, pos: 1}, {text: "+", kind: tokenOp, pos: 2}, {kind: tokenEOF, pos: 3}}, 0},
		{"a--b", []lexToken{{text: "a", kind: tokenIdent, pos: 1}, {text: "-", kind: tokenOp, pos: 2}, {text: "-", kind: tokenOp, pos: 3}, {text: "b", kind: tokenIdent, pos: 4}, {kind: tokenEOF, pos: 5}}, 0},
		// brackets
		{"()", []lexToken{{text: "(", kind: tokenOpen, pos: 1}, {text: ")", kind: tokenClose, pos: 2}, {kind: tokenEOF, pos: 3}}, 0},
		{"[]", []lexToken{{text: "[", kind: tokenOpen, pos: 1}, {text: "]", kind: tokenClose, pos: 2}, {kind: tokenEOF, pos: 3}}, 0},
		{"{}", []lexToken{{text: "{", kind: tokenOpen, pos: 1}, {text: "}", kind: tokenClose, pos: 2}, {kind: tokenEOF, pos: 3}}, 0},
		// calls
		{"a, b", []lexToken{{text: "a", kind: tokenIdent, pos: 1}, {text: ",", kind: tokenSep, pos: 2}, {text: "b", kind: tokenIdent, pos: 4}, {kind: tokenEOF, pos: 5}}, 0},
		{"a; b", []lexToken{{text: "a", kind: tokenIdent, pos: 1}, {text: ";", kind: tokenSep, pos: 2}, {text: "b", kind: tokenIdent, pos: 4}, {kind: tokenEOF, pos: 5}}, 0},
		{"1, 2", []lexToken{{text: "1", kind: tokenNum, pos: 1}, {text: ",", kind: tokenSep, pos: 2}, {text: "2", kind: tokenNum, pos: 4}, {kind: tokenEOF, pos: 5}}, 0},
		{"1; 2", []lexToken{{text: "1", kind: tokenNum, pos: 1}, {text: ";", kind: tokenSep, pos: 2}, {text: "2", kind: tokenNum, pos: 4}, {kind: tokenEOF, pos: 5}}, 0},
		// erroneous symbols
		{"$", []lexToken{{pos: 1}, {kind: tokenEOF, pos: 2}}, 1},
		{"a$", []lexToken{{text: "a", kind: tokenIdent, pos: 1}, {pos: 2}, {kind: tokenEOF, pos: 3}}, 1},
		{"$a", []lexToken{{pos: 1}, {text: "a", kind: tokenIdent, pos: 2}, {kind: tokenEOF, pos: 3}}, 1},
		{"0$", []lexToken{{pos: 1}, {kind: tokenEOF, pos: 3}}, 1},
		{"$0", []lexToken{{pos: 1}, {text: "0", kind: tokenNum, pos: 2}, {kind: tokenEOF, pos: 3}}, 1},
		{"$$", []lexToken{{pos: 1}, {pos: 2}, {kind: tokenEOF, pos: 3}}, 2},
		// cases identified with fuzzing
		{"0E0.", []lexToken{{pos: 1}, {kind: tokenEOF, pos: 5}}, 1},
	}

	for _, c := range cases {
		scan := lex(strings.NewReader(c.src))
		for _, want := range c.tokens {
			got, err := scan.next("")
			if err == io.EOF {
				t.Errorf("scanning %q: expected token %v but got EOF", c.src, want)
				continue
			}
			if got != want {
				t.Errorf("scanning %q: want %v, got %v", c.src, want, got)
			}
			if err != nil {
				if c.errs > 0 {
					c.errs--
					continue
				}
				t.Errorf("scanning %q: unexpected error %v", c.src, err)
			}
		}
		for got, err := scan.next(""); err != io.EOF; got, err = scan.next("") {
			if c.errs > 0 {
				c.errs--
			}
			t.Errorf("scanning %q: extra token %v with error: %v", c.src, got, err)
		}
		if c.errs > 0 {
			t.Errorf("scanning %q: not enough errors", c.src)
		}
	}
}

func TestLexMulti(t *testing.T) {
	cases := []struct {
		name   string
		src    string
		ws     string
		tokens [][]lexToken
	}{
		{"newline", "x\nx", "\n", [][]lexToken{{{text: "x", kind: tokenIdent, pos: 1}, {kind: tokenEOF, pos: 2}}, {{text: "x", kind: tokenIdent, pos: 1}, {kind: tokenEOF, pos: 2}}}},
		{"num", "1\n1", "\n", [][]lexToken{{{text: "1", kind: tokenNum, pos: 1}, {kind: tokenEOF, pos: 2}}, {{text: "1", kind: tokenNum, pos: 1}, {kind: tokenEOF, pos: 2}}}},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			src := strings.NewReader(c.src)
			for _, tokens := range c.tokens {
				scan := lex(src)
				for i, want := range tokens {
					got, err := scan.next(c.ws)
					if err != nil {
						t.Errorf("scanning %q up to %q, iter %d: unexpected error %v", c.src, c.ws, i, err)
					}
					if got != want {
						t.Errorf("scanning %q up to %q, iter %d: want %v, got %v", c.src, c.ws, i, want, got)
					}
				}
			}
		})
	}
}
