package expressions

import (
	"strconv"
	"unicode"
)

// ParseOption is an option for parsing.
type ParseOption interface {
	parseOption(parsectx) parsectx
}

type (
	funcopt struct {
		name string
		fn   Func
	}
	funcsopt map[string]Func
	eofopt   struct {
		c, s bool
		ws   string
	}
)

// parsectx holds general data for parsing. It is also a ParseOption.
type parsectx struct {
	// names is the set of variable names that have been seen this parse.
	names map[string]bool
	// funcs is the set of function names that trigger special parsing for ids.
	funcs map[string]Func
	// resv is a reserved parsed node. parsearglist sets this when it parses a
	// single parenthesized term so that the parser can back it out to an
	// implicit multiplication if the function is niladic.
	resv *node
	// wseof is a string containing the whitespace characters that trigger an
	// EOF token from the lexer.
	wseof string
	// ceof and seof indicate whether commas and semicolons, respectively, are
	// allowed at the end of an expression.
	ceof, seof bool
	// nodefaults indicates that parse options have set all default functions.
	nodefaults bool
}

func (p *parsectx) checkdefaults() {
	if p.nodefaults {
		return
	}
	n := 0
	for k := range p.funcs {
		if _, ok := globalfuncs[k]; ok {
			n++
		}
	}
	if n == len(globalfuncs) {
		p.nodefaults = true
	}
}

// ParseFunc sets a function for parsing. To disable parsing a function, pass
// nil for fn.
func ParseFunc(name string, fn Func) ParseOption {
	return &funcopt{name, fn}
}

func (o *funcopt) parseOption(p parsectx) parsectx {
	if p.funcs == nil {
		p.funcs = map[string]Func{}
	}
	p.funcs[o.name] = o.fn
	return p
}

// ParseFuncs sets a group of functions for parsing. To disable parsing any
// function, set it to nil.
func ParseFuncs(fns map[string]Func) ParseOption {
	return funcsopt(fns)
}

func (o funcsopt) parseOption(p parsectx) parsectx {
	if p.funcs == nil {
		// Always make a copy.
		p.funcs = make(map[string]Func, len(o))
	}
	for k, v := range o {
		p.funcs[k] = v
	}
	p.checkdefaults()
	return p
}

// DisableDefaultFuncs disables all default functions during parsing. Their
// names will be parsed as variables instead.
func DisableDefaultFuncs() ParseOption {
	return disablefns
}

var disablefns = funcsopt{
	"exp":   nil,
	"ln":    nil,
	"log":   nil,
	"sqrt":  nil,
	"cos":   nil,
	"sin":   nil,
	"tan":   nil,
	"acos":  nil,
	"asin":  nil,
	"atan":  nil,
	"cosh":  nil,
	"sinh":  nil,
	"tanh":  nil,
	"acosh": nil,
	"asinh": nil,
	"atanh": nil,
	"pi":    nil,
	"e":     nil,
}

// StopOn tells the parser to treat a list of characters as ending the
// expression. Each rune must be a comma, semicolon, or whitespace codepoint.
// Whitespace does not end an expression where a term is expected, e.g. at the
// beginning of an expression or following an operator or bracket. Commas and
// semicolons do not end expressions inside bracketed function argument lists.
//
// StopOn overrides the effect of any previous StopOn in the parsing options,
// including in presets. With no arguments, StopOn produces the default
// termination behavior, which is to parse to EOF.
func StopOn(chars ...rune) ParseOption {
	var o eofopt
	v := make([]rune, 0, len(chars))
	have := func(r rune) bool {
		for _, c := range v {
			if r == c {
				return true
			}
		}
		return false
	}
	for _, r := range chars {
		switch {
		case r == ',':
			o.c = true
		case r == ';':
			o.s = true
		case unicode.IsSpace(r):
			if have(r) {
				continue
			}
			v = append(v, r)
		default:
			panic("expressions: cannot stop on " + strconv.QuoteRune(r))
		}
	}
	o.ws = string(v)
	return &o
}

func (o *eofopt) parseOption(p parsectx) parsectx {
	p.ceof = o.c
	p.seof = o.s
	p.wseof = o.ws
	return p
}

// ParsingPreset creates a parsing preset that may be more efficient when using
// the same non-default parsing options for many calls to Parse. A preset
// panics when it would change any option from the default, but it is safe to
// apply other options after a preset.
func ParsingPreset(opts ...ParseOption) ParseOption {
	var p parsectx
	for _, opt := range opts {
		p = opt.parseOption(p)
	}
	if p.funcs != nil {
		// If we've set any functions, add unset default ones now.
		for k, v := range globalfuncs {
			if _, ok := p.funcs[k]; !ok {
				p.funcs[k] = v
			}
		}
		p.nodefaults = true
	}
	return &p
}

func (o *parsectx) parseOption(p parsectx) parsectx {
	if p.funcs != nil || p.wseof != "" || p.ceof || p.seof {
		panic("expressions: preset applied to non-default parse config")
	}
	p.funcs = o.funcs
	p.nodefaults = o.nodefaults
	return p
}
