package pins.data.symbol;

public enum Token {
	// Braces
	LEFT_PAREN,
	RIGHT_PAREN,
	LEFT_BRACE,
	RIGHT_BRACE,
	LEFT_BRACKET,
	RIGHT_BRACKET,

	//
	COMMA,
	COLON,
	SEMICOLON,

	// Logical
	AND,
	OR,
	NOT,

	// Arithmetic
	STAR,
	SLASH,
	MODULO,
	PLUS,
	MINUS,
	XOR,

	// Comparison
	EQUAL_EQUAL,
	LESS,
	GREAT,
	NOT_EQUAL,
	LESS_EQUAL,
	GREATER_EQUAL,

	// Other
	EQUAL,

	// Constants
	CONST_VOID,
	CONST_INT,
	CONST_CHAR,
	CONST_PTR,

	// Keywords
	CHAR,
	DEL,
	DO,
	ELSE,
	END,
	FUN,
	IF,
	INT,
	NEW,
	THEN,
	TYP,
	VAR,
	VOID,
	WHERE,
	WHILE,

	IDENTIFIER,

	// Special
	EOF,
	;


}
