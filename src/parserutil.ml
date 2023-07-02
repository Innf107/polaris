open Parser

let pretty_token = function
| IDENT i -> "IDENT(" ^ i ^ ")"
| CONSTRUCTOR i -> "CONSTRUCTOR(" ^ i ^ ")"
| STRING str -> "STRING(" ^ str ^ ")"
| INT i -> "INT(" ^ Int.to_string i ^ ")" 
| FLOAT f -> "FLOAT"
| INTERP_STRING_START -> "INTERP_STRING_START"
| STRING_COMPONENT str -> "STRING_COMPONENT(" ^ str ^ ")"
| INTERPOLATION_START -> "INTERPOLATION_START"
| INTERPOLATION_END -> "INTERPOLATION_END"
| INTERP_STRING_END -> "INTERP_STRING_END"
| LET -> "LET"
| TRUE -> "TRUE" 
| FALSE -> "FALSE"
| LAMBDA -> "LAMBDA" 
| ARROW -> "ARROW"
| LARROW -> "LARROW"
| DOT -> "DOT"
| COMMA -> "COMMA" 
| SEMI  -> "SEMI"
| COLON -> "COLON"
| DOUBLECOLON -> "DOUBLECOLON"
| LPAREN -> "LPAREN" 
| RPAREN -> "RPAREN" 
| LBRACE -> "LBRACE" 
| RBRACE -> "RBRACE" 
| LBRACKET -> "LBRACKET" 
| RBRACKET -> "RBRACKET"
| EQUALS -> "EQUALS" 
| COLONEQUALS -> "COLONEQUALS"
| BANGEQUALS -> "BANGEQUALS" 
| DOUBLEEQUALS -> "DOUBLEEQUALS"
| LT -> "LT"
| GT -> "GT"
| LE -> "LE"
| GE -> "GE"
| PLUS -> "PLUS" 
| MINUS -> "MINUS"
| STAR -> "STAR"
| SLASH -> "SLASH"
| TILDE -> "TILDE"
| DDOT ->  "DDOT"
| OR -> "OR" 
| AND -> "AND" 
| NOT -> "NOT"
| BANG -> "BANG"
| PROGCALL str -> "PROGCALL(" ^ str ^ ")"
| ENVVAR str -> "ENVVAR(" ^ str ^ ")"
| PIPE -> "PIPE"
| IF -> "IF" 
| THEN -> "THEN" 
| ELSE -> "ELSE"
| ASYNC -> "ASYNC" 
| AWAIT -> "AWAIT"
| MATCH -> "MATCH"
| EOF -> "EOF"
| USAGE -> "USAGE"
| DESCRIPTION -> "DESCRIPTION"
| OPTIONS -> "OPTIONS"
| AS -> "AS"
| WITH -> "WITH"
| EXTEND -> "EXTEND"
| MODULE -> "MODULE"
| IMPORT -> "IMPORT"
| EXPORT -> "EXPORT"
| FORALL -> "FORALL"
| DATA -> "DATA"
| TYPE -> "TYPE"
| BACKTICK -> "BACKTICK"
| REF -> "REF"
| EXCEPTION -> "EXCEPTION"
| TRY -> "TRY"
| RAISE -> "RAISE"
| INSTANCE -> "INSTANCE"
| CLASS -> "CLASS"
