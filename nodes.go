package expressions

import (
	"strings"
)

// node is a node in the abstract syntax tree of an expression.
type node struct {
	kind nodeKind

	name string
	fn   Func

	left  *node
	right *node
}

type nodeKind int8

const (
	nodeNone nodeKind = iota

	nodeNum  // push num
	nodeName // push lookup(name)

	// TODO(zeph): how represent e.g. 0F0(; ; z) = exp(z)
	nodeCall // name is Func to call, right is link to nodeArg unless niladic
	nodeArg  // name is "" or "," or ";", eval left, right is link to next arg

	nodeNeg // evaluate left, then negate
	nodeAdd // evaluate left, add right
	nodeSub // evaluate left, sub right
	nodeMul // evaluate left, mul right
	nodeDiv // evaluate left, div by right
	nodePow // evaluate left, exp by right
	nodeNop // evaluate left
)

//go:generate go mod edit -require=golang.org/x/tools@v0.1.0
//go:generate go mod download
//go:generate go run golang.org/x/tools/cmd/stringer -type=nodeKind -trimprefix=node
//go:generate go mod tidy

func (n *node) String() string {
	var b strings.Builder
	n.fmt(&b, false, false)
	return b.String()
}

func (n *node) fmt(b *strings.Builder, square, alt bool) {
	var l, r byte = '(', ')'
	if square {
		l, r = '[', ']'
	}
	b.WriteByte(l)
	defer b.WriteByte(r)
	switch n.kind {
	case nodeNone:
		// Invalid nodes use invalid characters.
		b.WriteByte('$')
		if n.left != nil {
			n.left.fmt(b, square, alt)
		}
		b.WriteByte('#')
		if n.right != nil {
			n.right.fmt(b, square, alt)
		}
		b.WriteByte('$')
	case nodeNum, nodeName:
		b.WriteString(n.name)
	case nodeCall:
		b.WriteString(n.name)
		n.fmtargs(b, !square, alt)
	case nodeArg:
		// Args usually only appear inside calls, which are handled by fmtargs.
		b.WriteByte(':')
		n.left.fmt(b, !square, alt)
		if n.right != nil {
			n.right.fmt(b, !square, alt)
		}
	case nodeNeg:
		b.WriteByte('-')
		n.left.fmt(b, !square, alt)
	case nodeAdd:
		n.left.fmt(b, !square, alt)
		b.WriteString(" + ")
		n.right.fmt(b, !square, alt)
	case nodeSub:
		n.left.fmt(b, !square, alt)
		b.WriteString(" - ")
		n.right.fmt(b, !square, alt)
	case nodeMul:
		n.left.fmt(b, !square, alt)
		if !alt {
			b.WriteString(" * ")
		} else {
			b.WriteString(" × ")
		}
		n.right.fmt(b, !square, alt)
	case nodeDiv:
		n.left.fmt(b, !square, alt)
		if !alt {
			b.WriteString(" / ")
		} else {
			b.WriteString(" ÷ ")
		}
		n.right.fmt(b, !square, alt)
	case nodePow:
		n.left.fmt(b, !square, alt)
		b.WriteString(" ^ ")
		n.right.fmt(b, !square, alt)
	case nodeNop:
		b.WriteByte('+')
		n.left.fmt(b, !square, alt)
	default:
		panic("expressions: invalid node kind " + n.kind.String() + " after writing " + b.String())
	}
}

func (n *node) fmtargs(b *strings.Builder, square, alt bool) {
	var l, r byte = '(', ')'
	if square {
		l, r = '[', ']'
	}
	b.WriteByte(l)
	defer b.WriteByte(r)
	if n.right == nil {
		// Niladic call.
		return
	}
	n = n.right
	if n.kind != nodeArg {
		b.WriteString("***")
		n.fmt(b, !square, alt)
		return
	}
	n.left.fmt(b, !square, alt)
	for n.right != nil {
		n = n.right
		if n.kind != nodeArg {
			b.WriteString("***")
			n.fmt(b, !square, alt)
			return
		}
		b.WriteString(", ")
		n.left.fmt(b, !square, alt)
	}
}
