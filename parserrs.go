package expressions

import "strconv"

// OperatorError is an error indicating an operator token that is not
// understood by the parser. It implements InputError.
type OperatorError struct {
	// Col is the position of the operator.
	Col int
	// Operator is the token that was not understood.
	Operator string
	// Unary is whether the parser expected a unary operator at the time.
	Unary bool
}

func (err *OperatorError) Error() string {
	s := "binary"
	if err.Unary {
		s = "unary"
	}
	return errpos(err.Col, "unknown "+s+" operator "+strconv.Quote(err.Operator))
}

func (err *OperatorError) Pos() int {
	return err.Col
}

// BracketError is an error indicating mismatched brackets in the
// input. It implements InputError.
type BracketError struct {
	// Col is the position of the operator.
	Col int
	// Left is the opening bracket.
	Left string
	// Right is the mismatched closing bracket.
	Right string
}

func (err *BracketError) Error() string {
	if err.Left == "" {
		return errpos(err.Col, "close bracket "+err.Right+" with no open bracket")
	}
	if err.Right == "" {
		return errpos(err.Col, "open bracket "+err.Left+" with no close bracket")
	}
	return errpos(err.Col, "mismatched bracket: "+err.Left+"expr"+err.Right)
}

func (err *BracketError) Pos() int {
	return err.Col
}

// SeparatorError is an error indicating an illegal use of a comma or semicolon
// separator. It implements InputError.
type SeparatorError struct {
	// Col is the position of the separator.
	Col int
	// Sep is the separator.
	Sep string
}

func (err *SeparatorError) Error() string {
	return errpos(err.Col, "invalid occurrence of separator "+strconv.Quote(err.Sep))
}

func (err *SeparatorError) Pos() int {
	return err.Col
}

// CallError is an error indicating a function call with the wrong number of
// arguments. It implements InputError.
type CallError struct {
	// Col is the position of the end of the call expression.
	Col int
	// Func is the function name that was called.
	Func string
	// Len is the number of arguments the function call tried to imply.
	Len int
}

func (err *CallError) Error() string {
	return errpos(err.Col, "cannot call "+err.Func+" with "+strconv.Itoa(err.Len)+" arguments")
}

func (err *CallError) Pos() int {
	return err.Col
}

// EmptyExpressionError is an error indicating an empty subexpression.
type EmptyExpressionError struct {
	// Col is the position of the token that ended the subexpression.
	Col int
	// End is the token that ended the subexpression.
	End string
}

func (err *EmptyExpressionError) Error() string {
	if err.End == "" {
		if err.Col <= 1 {
			return errpos(err.Col, "no expression")
		}
		return errpos(err.Col, "no expression at end")
	}
	return errpos(err.Col, "no expression up to "+strconv.Quote(err.End))
}

func (err *EmptyExpressionError) Pos() int {
	return err.Col
}

// errpos is a shortcut to create an error message with a position.
func errpos(pos int, msg string) string {
	return strconv.Itoa(pos) + ": " + msg
}

// InputError is an error with position information. Every error resulting from
// invalid input implements InputError.
type InputError interface {
	error
	// Pos returns the position of the error as the number of runes up to and
	// including the start of the token that caused the error.
	Pos() int
}

var (
	_ InputError = (*OperatorError)(nil)
	_ InputError = (*BracketError)(nil)
	_ InputError = (*SeparatorError)(nil)
	_ InputError = (*CallError)(nil)
	_ InputError = (*EmptyExpressionError)(nil)
	_ InputError = (*LexError)(nil)
)
