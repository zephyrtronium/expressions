// Code generated by "stringer -type=tokenKind -trimprefix=token"; DO NOT EDIT.

package expressions

import "strconv"

func _() {
	// An "invalid array index" compiler error signifies that the constant values have changed.
	// Re-run the stringer command to generate them again.
	var x [1]struct{}
	_ = x[tokenNone-0]
	_ = x[tokenNum-1]
	_ = x[tokenIdent-2]
	_ = x[tokenOp-3]
	_ = x[tokenBracket-4]
}

const _tokenKind_name = "NoneNumIdentOpBracket"

var _tokenKind_index = [...]uint8{0, 4, 7, 12, 14, 21}

func (i tokenKind) String() string {
	if i < 0 || i >= tokenKind(len(_tokenKind_index)-1) {
		return "tokenKind(" + strconv.FormatInt(int64(i), 10) + ")"
	}
	return _tokenKind_name[_tokenKind_index[i]:_tokenKind_index[i+1]]
}
