grammar WShExDoc;

wShExDoc: directive* statement* EOF;

directive: baseDecl | prefixDecl | importDecl;

baseDecl: KW_BASE IRIREF;

prefixDecl: KW_PREFIX PNAME_NS IRIREF;

importDecl: KW_IMPORT iri;

statement: start | shapeExprDecl;

start: KW_START '=' shapeExpression;

shapeExprDecl: shapeExprLabel shapeExpression;

shapeExpression: shapeOr;

inlineShapeExpression: inlineShapeOr;

shapeOr: shapeAnd (KW_OR shapeAnd)*;

inlineShapeOr: inlineShapeAnd (KW_OR inlineShapeAnd)*;

shapeAnd: shapeNot (KW_AND shapeNot)*;

inlineShapeAnd: inlineShapeNot (KW_AND inlineShapeNot)*;

shapeNot: negation? shapeAtom;

inlineShapeNot: negation? inlineShapeAtom;

negation: KW_NOT | '!';

shapeAtom:
	nonLitNodeConstraint shapeOrRef?	# shapeAtomNonLitNodeConstraint
	| litNodeConstraint					# shapeAtomLitNodeConstraint
	| shapeOrRef nonLitNodeConstraint?	# shapeAtomShapeOrRef
	| '(' shapeExpression ')'			# shapeAtomShapeExpression
	| any								# shapeAtomAny; // no constraint

inlineShapeAtom:
	inlineNonLitNodeConstraint inlineShapeOrRef?	# inlineShapeAtomNonLitNodeConstraint
	| inlineLitNodeConstraint						# inlineShapeAtomLitNodeConstraint
	| inlineShapeOrRef inlineNonLitNodeConstraint?	# inlineShapeAtomShapeOrRef
	| '(' shapeExpression ')'						# inlineShapeAtomShapeExpression
	| any											# inlineShapeAtomAny; // no constraint

shapeOrRef: shapeDefinition | shapeRef;

inlineShapeOrRef: inlineShapeDefinition | shapeRef;

shapeRef: ATPNAME_LN | ATPNAME_NS | '@' shapeExprLabel;

inlineLitNodeConstraint
	: literalKind xsFacet* # nodeConstraintLiteral 
	//W | nonLiteralKind stringFacet* # nodeConstraintNonLiteral 
	| datatype xsFacet*   # nodeConstraintDatatype
	| valueSet xsFacet*   # nodeConstraintValueSet  
	| numericFacet+		  # nodeConstraintNumericFacet
	;

litNodeConstraint:
 	inlineLitNodeConstraint; //W annotation* semanticAction*

inlineNonLitNodeConstraint:
	//W  nonLiteralKind stringFacet* # litNodeConstraintLiteral |
	 stringFacet+ # litNodeConstraintStringFacet
	;

nonLitNodeConstraint:
	inlineNonLitNodeConstraint; //W annotation* semanticAction*

nonLiteralKind
    : KW_IRI 
    //W | KW_BNODE 
	| KW_NONLITERAL 
	;

literalKind
    : KW_LITERAL
	| KW_TIME	
	| KW_QUANTITY
	| KW_STRING
	| KW_MONOLINGUALTEXT
	| KW_MULTILINGUALTEXT
	| KW_GEOCOORDINATES
	| KW_GEOSHAPE
	| KW_MEDIA
	;

xsFacet: stringFacet | numericFacet;

stringFacet: stringLength INTEGER 
           | REGEXP REGEXP_FLAGS?
		   ;

stringLength: KW_LENGTH | KW_MINLENGTH | KW_MAXLENGTH;

numericFacet: numericRange rawNumeric | numericLength INTEGER;

numericRange:
	KW_MININCLUSIVE
	| KW_MINEXCLUSIVE
	| KW_MAXINCLUSIVE
	| KW_MAXEXCLUSIVE;

numericLength: KW_TOTALDIGITS | KW_FRACTIONDIGITS;

// rawNumeric is like numericLiteral but returns a JSON integer or float
rawNumeric: INTEGER | DECIMAL | DOUBLE;

shapeDefinition:
	inlineShapeDefinition; //W annotation* semanticAction*

inlineShapeDefinition: qualifier* '{' tripleExpression? '}';

qualifier:
	extension
	| restriction
	| extraPropertySet
	| labelConstraint
	| descriptionConstraint 
	| aliasConstraint
	| KW_CLOSED;

extraPropertySet: KW_EXTRA predicate+;

labelConstraint: KW_LABEL langConstraints;

langConstraints: '(' (singleLangConstraint | multiLangConstraint) ')'
	;

singleLangConstraint : langConstraint ','? 
                     ;

multiLangConstraint : langConstraint (',' langConstraint)+ ','? 
                    ;

descriptionConstraint: KW_DESCRIPTION langConstraints;

aliasConstraint: KW_ALIAS langConstraints;

langConstraint: LANGLABEL '->' stringConstraint;

stringConstraint: any  
                | stringSet 
                | stringFacet
				;

tripleExpression: oneOfTripleExpr;

oneOfTripleExpr: groupTripleExpr | multiElementOneOf;

multiElementOneOf: groupTripleExpr ( '|' groupTripleExpr)+;

groupTripleExpr: singleElementGroup | multiElementGroup;

any: '.';


singleElementGroup: unaryTripleExpr ';'?;

multiElementGroup: unaryTripleExpr (';' unaryTripleExpr)+ ';'?;

unaryTripleExpr:
	//W ('$' tripleExprLabel)? 
	(tripleConstraint | bracketedTripleExpr)
	//W | include W | expr
	;

bracketedTripleExpr: '(' tripleExpression ')' cardinality?;
	//W annotation* semanticAction*

tripleConstraint:
		//W senseFlags? 
		predicate inlineShapeExpression cardinality? propertySpec? referencesSpec?;
	//W annotation* semanticAction* /* variableDecl? */

cardinality:
		'*'				# starCardinality
		| '+'			# plusCardinality
		| '?'			# optionalCardinality
		| repeatRange	# repeatCardinality;

repeatRange:
		'{' INTEGER '}'						# exactRange
		| '{' min_range ',' max_range? '}'	# minMaxRange;

min_range: INTEGER;

max_range: INTEGER | '*';

propertySpec: '{|' predicate shapeAtom cardinality? '|}';

referencesSpec: KW_REFERENCES oneOfReferencesExpr ;

oneOfReferencesExpr: singleReferencesExpr ( '|' oneOfReferencesExpr )* ;

singleReferencesExpr: propertySpec cardinality? ;

basicExpr:
		literal
		| iri
		| blankNode;

	senseFlags: '!' '^'? | '^' '!'?;

valueSet: '[' valueSetValue* ']';

// WShEx
stringSet: string* 
         ;

valueSetValue:
		iriRange
		//W | literalRange W | languageRange W | '.' ( W iriExclusion+ W | literalExclusion+ W |
		// languageExclusion+ W )
		;

iriRange: iri;
		//W (STEM_MARK iriExclusion*)?

		//W iriExclusion: '-' iri STEM_MARK?;

		//W literalRange: literal (STEM_MARK literalExclusion*)?;

		//W literalExclusion: '-' literal STEM_MARK?;

		//W languageRange: LANGTAG (STEM_MARK languageExclusion*)? # languageRangeFull | '@'
		// STEM_MARK languageExclusion* # languageRangeAt;

		//W languageExclusion: '-' LANGTAG STEM_MARK?;

		//W include: '&' tripleExprLabel;

		//W annotation: '//' predicate (iri | literal);

		//W semanticAction: '%' iri (CODE | '%');

		literal: rdfLiteral | numericLiteral | booleanLiteral;

		// BNF: predicate ::= iri | RDF_TYPE
		/* predicate
 : iri
 | rdfType
 ;
		 */
predicate: iri;
		// Removed RDF_TYPE as predicate (not allowed in WShEx)

// This one could be removed
rdfType: RDF_TYPE;

datatype: iri;

shapeExprLabel: iri | blankNode;

tripleExprLabel: iri | blankNode;

numericLiteral: INTEGER | DECIMAL | DOUBLE;

rdfLiteral: string (LANGTAG | '^^' datatype)?;

// WShEx
stringLiteral: string LANGTAG?;

booleanLiteral: KW_TRUE | KW_FALSE;

string:
			STRING_LITERAL_LONG1
			| STRING_LITERAL_LONG2
			| STRING_LITERAL1
			| STRING_LITERAL2;

iri: IRIREF | prefixedName;

prefixedName: PNAME_LN | PNAME_NS;

blankNode: BLANK_NODE_LABEL;

extension: KW_EXTENDS shapeRef+ | '&' shapeRef+;

restriction: KW_RESTRICTS shapeRef+ | '-' shapeRef+;

// Keywords
KW_ABSTRACT: A B S T R A C T;

KW_AS: A S;

KW_BASE: B A S E;

KW_EXTENDS: E X T E N D S;

KW_IMPORT: I M P O R T;

KW_RESTRICTS: R E S T R I C T S;

KW_EXTERNAL: E X T E R N A L;

KW_PREFIX: P R E F I X;

KW_START: S T A R T;

KW_VIRTUAL: V I R T U A L;

KW_CLOSED: C L O S E D;

KW_EXTRA: E X T R A;

KW_LABEL: L A B E L;

KW_DESCRIPTION: D E S C R I P T I O N;

KW_ALIAS: A L I A S;

KW_LITERAL: L I T E R A L;

KW_IRI: I R I;

KW_NONLITERAL: N O N L I T E R A L;

KW_BNODE: B N O D E;

KW_AND: A N D;

KW_OR: O R;

KW_MININCLUSIVE: M I N I N C L U S I V E;

KW_MINEXCLUSIVE: M I N E X C L U S I V E;

KW_MAXINCLUSIVE: M A X I N C L U S I V E;

KW_MAXEXCLUSIVE: M A X E X C L U S I V E;

KW_LENGTH: L E N G T H;

KW_MINLENGTH: M I N L E N G T H;

KW_MAXLENGTH: M A X L E N G T H;

KW_TOTALDIGITS: T O T A L D I G I T S;

KW_FRACTIONDIGITS: F R A C T I O N D I G I T S;

KW_NOT: N O T;

KW_TRUE: 'true';

KW_FALSE: 'false';

KW_TIME: T I M E ;

KW_QUANTITY: Q U A N T I T Y ;

KW_STRING: S T R I N G ;

KW_MONOLINGUALTEXT: M O N O L I N G U A L T E X T ;

KW_MULTILINGUALTEXT: M U L T I L I N G U A L T E X T ;

KW_GEOCOORDINATES: G E O C O O R D I N A T E S ;

KW_GEOSHAPE: G E O S H A P E ;

KW_MEDIA: M E D I A ;

KW_REFERENCES: R E F E R E N C E S ;

// -------------------------- TERMINALS --------------------------

// Skip white spaces in the shEx and comments.
SKIP_: (WHITE_SPACE | COMMENT) -> skip;

fragment COMMENT: (
		'#' ~[\r\n]*
		| '/*' (~[*] | '*' ('\\/' | ~[/]))* '*/'
	);

// A white space is defined as '\t' or '\r' or '\n'.
fragment WHITE_SPACE: [ \t\r\n]+;

CODE: '{' (~[%\\] | '\\' [%\\] | UCHAR)* '%' '}';


RDF_TYPE: 'a';

IRIREF: '<' (~[\u0000-\u0020=<>"{}|^`\\] | UCHAR)* '>';

PNAME_NS: PN_PREFIX? ':';

PNAME_LN: PNAME_NS PN_LOCAL;

ATPNAME_NS: '@' PN_PREFIX? ':';

ATPNAME_LN: '@' PNAME_NS PN_LOCAL;

REGEXP:
			'/' (
				~[/\n\r\\]
				| '\\' [/nrt\\|.?*+(){}[\]$^-]
				| UCHAR
			)+ '/';

REGEXP_FLAGS: [smix]+;

BLANK_NODE_LABEL:
			'_:' (PN_CHARS_U | [0-9]) (
				(PN_CHARS | '.')* PN_CHARS
			)?;

LANGTAG: '@' LANGLABEL
       ;

LANGLABEL: [a-zA-Z]+ ('-' [a-zA-Z0-9]+)* 
         ;

INTEGER: [+-]? [0-9]+;

DECIMAL: [+-]? [0-9]* '.' [0-9]+;

DOUBLE:	[+-]? (
				[0-9]+ '.' [0-9]* EXPONENT
				| '.'? [0-9]+ EXPONENT
			);

STEM_MARK: '~';

UNBOUNDED: '*';

fragment EXPONENT: [eE] [+-]? [0-9]+;

STRING_LITERAL1:
			'\'' (~[\u0027\u005C\u000A\u000D] | ECHAR | UCHAR)* '\'';
		/* #x27=' #x5C=\ #xA=new line #xD=carriage return */

STRING_LITERAL2:
			'"' (~[\u0022\u005C\u000A\u000D] | ECHAR | UCHAR)* '"';
		/* #x22=" #x5C=\ #xA=new line #xD=carriage return */

STRING_LITERAL_LONG1:
			'\'\'\'' (('\'' | '\'\'')? (~['\\] | ECHAR | UCHAR))* '\'\'\'';

STRING_LITERAL_LONG2:
			'"""' (('"' | '""')? (~["\\] | ECHAR | UCHAR))* '"""';

fragment UCHAR:
			'\\u' HEX HEX HEX HEX
			| '\\U' HEX HEX HEX HEX HEX HEX HEX HEX;

fragment ECHAR: '\\' [tbnrf\\"'];

fragment PN_CHARS_BASE:
			[A-Z]
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
			| [\u{10000}-\u{EFFFD}];
		// | [\uD800-\uDB7F] [\uDC00-\uDFFF]

fragment PN_CHARS_U: PN_CHARS_BASE | '_';

fragment PN_CHARS:
			PN_CHARS_U
			| '-'
			| [0-9]
			| [\u00B7]
			| [\u0300-\u036F]
			| [\u203F-\u2040];

fragment PN_PREFIX:
			PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?;

fragment PN_LOCAL: (PN_CHARS_U | ':' | [0-9] | PLX) (
				(PN_CHARS | '.' | ':' | PLX)* (
					PN_CHARS
					| ':'
					| PLX
				)
			)?;

fragment PLX: PERCENT | PN_LOCAL_ESC;

fragment PERCENT: '%' HEX HEX;

fragment HEX: [0-9] | [A-F] | [a-f];

fragment PN_LOCAL_ESC:
			'\\' (
				'_'
				| '~'
				| '.'
				| '-'
				| '!'
				| '$'
				| '&'
				| '\''
				| '('
				| ')'
				| '*'
				| '+'
				| ','
				| ';'
				| '='
				| '/'
				| '?'
				| '#'
				| '@'
				| '%'
			);

		/*
		 VARNAME
 : ( PN_CHARS_U | DIGIT ) ( PN_CHARS_U | DIGIT | '\u00B7' | ('\u0300'..'\u036F') |
		 ('\u203F'..'\u2040') )*
 ;
		 */

/* fragment DIGIT: '0'..'9' ; */
fragment A: ('a' | 'A');
fragment B: ('b' | 'B');
fragment C: ('c' | 'C');
fragment D: ('d' | 'D');
fragment E: ('e' | 'E');
fragment F: ('f' | 'F');
fragment G: ('g' | 'G');
fragment H: ('h' | 'H');
fragment I: ('i' | 'I');
fragment J: ('j' | 'J');
fragment K: ('k' | 'K');
fragment L: ('l' | 'L');
fragment M: ('m' | 'M');
fragment N: ('n' | 'N');
fragment O: ('o' | 'O');
fragment P: ('p' | 'P');
fragment Q: ('q' | 'Q');
fragment R: ('r' | 'R');
fragment S: ('s' | 'S');
fragment T: ('t' | 'T');
fragment U: ('u' | 'U');
fragment V: ('v' | 'V');
fragment W: ('w' | 'W');
fragment X: ('x' | 'X');
fragment Y: ('y' | 'Y');
fragment Z: ('z' | 'Z');
