// ANTLR4 grammar for ShapePath
// https://shexspec.github.io/spec/ShExPath#grammar

grammar ShapePathDoc;

shapePathDoc
 : shapePathExpr
 ;

shapePathExpr
 : absolutePathExpr
 | relativePathExpr
 ;

absolutePathExpr
 : KW_SLASH  relativePathExpr
 ;

relativePathExpr
 : stepExpr (KW_SLASH stepExpr) *
 ;

stepExpr
 : contextTest ? exprIndex   # exprIndexStep
 | contextTest               # contextStep
 ;

contextTest
 : shapeExprContext
 | tripleExprContext
 ;

shapeExprContext
 : KW_ShapeAnd
 | KW_ShapeOr
 | KW_ShapeNot
 | KW_NodeConstraint
 | KW_Shape
 ;

tripleExprContext
 : KW_EachOf
 | KW_OneOf
 | KW_TripleConstraint
 ;

exprIndex
 : shapeExprIndex
 | tripleExprIndex
 ;

shapeExprIndex
 : '@' (INTEGER | shapeExprLabel)
 ;

tripleExprIndex
 : INTEGER | tripleExprLabel
 ;

shapeExprLabel
 : iri
 | blankNodeLabel
 ;

tripleExprLabel
 : (iri | blankNodeLabel) INTEGER?
 ;

blankNodeLabel
 : blankNode
 ;

iri
 : IRIREF
 | prefixedName
 ;

prefixedName
 : PNAME_LN
 | PNAME_NS
 ;

blankNode
 : BLANK_NODE_LABEL
 ;

MapsTo
 : '~>'
 ;

KW_PREFIX
 : P R E F I X
 ;

KW_ShapeAnd
 : S H A P E A N D
 ;

KW_ShapeOr
 : S H A P E O R
 ;

KW_ShapeNot
 : S H A P E N O T
 ;

KW_NodeConstraint
 : N O D E C O N S T R A I N T
 ;

KW_EachOf
 : E A C H O F
 ;

KW_OneOf
 : O N E O F
 ;

KW_Shape
 : S H A P E
 ;

KW_TripleConstraint
 : T R I P L E C O N S T R A I N T
 ;

KW_SLASH
 : '/'
 ;

 // --------------------------
 // TERMINALS
 // --------------------------

// Skip white spaces in the shEx and comments.
SKIP_
 : (WHITE_SPACE | COMMENT) -> skip
 ;

fragment COMMENT
 : ('#' ~[\r\n]* | '/*' (~[*] | '*' ('\\/' | ~[/]))* '*/') -> skip
 ;

// A white space is defined as '\t' or '\r' or '\n'.
fragment WHITE_SPACE
 : [ \t\r\n]+
 ;


IRIREF
 : '<' (~[\u0000-\u0020=<>"{}|^`\\] | UCHAR)* '>'
 ; /* #x00=NULL #01-#x1F=control codes #x20=space */

BLANK_NODE_LABEL
 : '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
 ;

PNAME_NS
 : PN_PREFIX? ':'
 ;

PNAME_LN
 : PNAME_NS PN_LOCAL
 ;

ATPNAME_NS
 : '@' PN_PREFIX? ':'
 ;

ATPNAME_LN
 : '@' PNAME_NS PN_LOCAL
 ;

INTEGER
 : [+-]? [0-9]+
 ;

fragment EXPONENT
 : [eE] [+-]? [0-9]+
 ;

STRING_LITERAL1
 : '\'' (~[\u0027\u005C\u000A\u000D] | ECHAR | UCHAR)* '\''
 ; /* #x27=' #x5C=\ #xA=new line #xD=carriage return */

STRING_LITERAL2
 : '"' (~[\u0022\u005C\u000A\u000D] | ECHAR | UCHAR)* '"'
 ;   /* #x22=" #x5C=\ #xA=new line #xD=carriage return */

STRING_LITERAL_LONG1
 : '\'\'\'' (('\'' | '\'\'')? (~['\\] | ECHAR | UCHAR))* '\'\'\''
 ;

STRING_LITERAL_LONG2
 : '"""' (('"' | '""')? (~["\\] | ECHAR | UCHAR))* '"""'
 ;

fragment UCHAR
 : '\\u' HEX HEX HEX HEX
 | '\\U' HEX HEX HEX HEX HEX HEX HEX HEX
 ;

fragment ECHAR
 : '\\' [tbnrf\\"']
 ;

fragment PN_CHARS_BASE
 : [A-Z]
 | [a-z]
 | [\u00C0-\u00D6]
 | [\u00D8-\u00F6]
 | [\u00F8-\u02FF]
 | [\u0370-\u037D]
 | [\u037F-\u1FFF]
 | [\u200C-\u200D]
 | [\u2070-\u218F]
 | [\u2C00-\u2FEF]
 | [\u3001-\uD7FF]
 | [\uF900-\uFDCF]
 | [\uFDF0-\uFFFD]
 | [\u{10000}-\u{EFFFD}]
 // | [\uD800-\uDB7F] [\uDC00-\uDFFF]
 ;

fragment PN_CHARS_U
 : PN_CHARS_BASE
 | '_'
 ;

fragment PN_CHARS
 : PN_CHARS_U
 | '-'
 | [0-9]
 | [\u00B7]
 | [\u0300-\u036F]
 | [\u203F-\u2040]
 ;

fragment PN_PREFIX
 : PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
 ;

fragment PN_LOCAL
 : (PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
 ;

fragment PLX
 : PERCENT
 | PN_LOCAL_ESC
 ;

fragment PERCENT
 : '%' HEX HEX
 ;

fragment HEX
 : [0-9]
 | [A-F]
 | [a-f]
 ;

fragment PN_LOCAL_ESC
 : '\\' ('_' | '~' | '.' | '-' | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%')
 ;

/*
VARNAME
 : ( PN_CHARS_U | DIGIT ) ( PN_CHARS_U | DIGIT | '\u00B7' | ('\u0300'..'\u036F') | ('\u203F'..'\u2040') )*
 ;
*/

/* fragment DIGIT: '0'..'9' ; */
fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');
