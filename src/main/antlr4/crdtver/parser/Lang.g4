grammar Lang;

program: NL? (declaration NL?)* NL* EOF;

declaration:
      procedure
    | typedecl
    | operationDecl
    | queryDecl
    | axiomDecl
    | invariant
    | crdtDecl
;


typedecl: kind=('idtype'|'type') name=ID typeParams? ('=' dataTypeCases+=dataTypeCase ('|' dataTypeCases+=dataTypeCase)*)?;

typeParams:
    '[' typeParam (',' typeParam)* ']';

typeParam:
    name=ID;


dataTypeCase: name=ID ('(' (params+=variable (',' params+=variable)*)? ')')?;

operationDecl: 'operation' name=ID '(' (params+=variable (',' params+=variable)*)? ')';

queryDecl: (inline='@inline')? 'query' name=ID '(' (params+=variable (',' params+=variable)*)? ')' ':' returnType=type
    ('=' implementation=expr | 'ensures' ensures=expr)?;

axiomDecl: 'axiom' expr;

procedure: 'def' name=ID '(' (params+=variable (',' params+=variable)*)? ')' (':' returnType=type)? NL body=stmt;

variable: name=ID (':' type)?;

crdtDecl: 'crdt' keyDecl;

keyDecl: name=ID ':' crdttype;

crdttype:
      structcrdt
    | crdt
    ;

structcrdt: (name=ID)? '{' keyDecl (',' keyDecl)* '}';

crdt: name=ID ('[' crdttype (','crdttype )* ']')?;

type: name=ID ('[' typeArgs+=type (',' typeArgs+=type)* ']')?;


stmt:
      blockStmt
    | atomicStmt
    | localVar
    | ifStmt
    | matchStmt
    | crdtCall
    | assignment
    | assertStmt
    | newIdStmt
    | returnStmt
    ;

blockStmt: STARTBLOCK stmt* ENDBLOCK;

assertStmt: 'assert' expr NL;

atomicStmt: 'atomic' NL stmt;

localVar: 'var' variable ('=' (expr | 'new' typename=ID))? NL;

ifStmt: 'if' condition=expr NL thenStmt=stmt ('else' NL? elseStmt=stmt)?;

matchStmt: expr 'match' NL STARTBLOCK cases+=matchCase* ENDBLOCK;

matchCase: 'case' expr '=>' NL STARTBLOCK stmt* ENDBLOCK;

crdtCall: 'call' functionCall NL;

assignment: varname=ID '=' expr NL;

newIdStmt: varname=ID '=' 'new' typename=ID  NL;

returnStmt: 'return' expr (asserts+=assertStmt)*  NL;

expr:
      varname=ID
    | boolval=('true'|'false')
    | intval=INT
    | receiver=expr '.' fieldName=ID
    | unaryOperator='!' right=expr
    | left=expr 'is' isAttribute='visible'
    | left=expr 'happened' operator=('before'|'after') right=expr
    | left=expr operator=('*'|'/'|'%') right=expr
    | left=expr operator=('+'|'-') right=expr
    | left=expr operator=('<'|'<='|'>'|'>=') right=expr
    | left=expr operator=('=='|'!=') right=expr
    | left=expr operator='&&' right=expr
    | left=expr operator='||' right=expr
    | <assoc=right> left=expr operator='==>' right=expr
    | quantifierExpr
    | 'forall valid snapshots' '::' validSnapshots=expr
    | functionCall
    | '(' parenExpr=expr ')'
    ;

quantifierExpr: quantifier=('forall'|'exists') vars+=variable (',' vars+=variable)* '::' expr;

functionCall: funcname=ID '(' (args+=expr (',' args+=expr)*)? ')';

invariant: free='free'? 'invariant' (name=ID ':')? expr;



// Names for keywords

PAREN_LEFT: '(';
PAREN_RIGHT: ')';
BRACKET_LEFT: '[';
BRACKET_RIGHT: ']';

STARTBLOCK:[()];
ENDBLOCK:[()];
INVALID:[()];


NOT: '!';
NOTEQ: '!=';
MOD: '%';
AND: '&&';
MULT: '*';
PLUS: '+';
COMMA: ',';
MINUS: '-';
DOT: '.';
DIV: '/';
COLON: ':';
COLONCOLON: '::';
LESS: '<';
LESSEQ: '<=';
EQ: '=';
EQEQ: '==';
IMPLIES: '==>';
ARROW: '=>';
GREATER: '>';
GREATEREQ: '>=';
BRACE_LEFT: '{';
BAR : '|';
OR: '||';
BRACE_RIGHT: '}';
AFTER: 'after';
ASSERT: 'assert';
ATOMIC: 'atomic';
AXIOM: 'axiom';
BEFORE: 'before';
CALL: 'call';
CASE: 'case';
CRDT: 'crdt';
DEF: 'def';
ELSE: 'else';
ENSURES: 'ensures';
EXISTS: 'exists';
FALSE: 'false';
FORALL: 'forall';
FORALL_VALID_SNAPSHOTS: 'forall valid snapshots';
FREE: 'free';
HAPPENED: 'happened';
IDTYPE: 'idtype';
IF: 'if';
INLINE: '@inline';
INVARIANT: 'invariant';
IS: 'is';
MATCH: 'match';
NEW: 'new';
OPERATION: 'operation';
QUERY: 'query';
RETURN: 'return';
TRUE: 'true';
TYPE: 'type';
VAR: 'var';
VISIBLE: 'visible';

// regular expression tokens:

ID: [a-zA-Z][a-zA-Z_0-9]*;
INT: '0'|[1-9][0-9]*;

ML_COMMENT: '/*' .*? '*/' -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
NL: [\r\n]+;
SPACETAB:' ' ' '+;
SPACES: ' ' -> skip;