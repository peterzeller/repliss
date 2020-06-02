grammar Lang;

ID: [a-zA-Z][a-zA-Z_0-9]*;
INT: '0'|[1-9][0-9]*;

ML_COMMENT: '/*' .*? '*/' -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
WS : [ \t\r\n]+ -> skip ;

program: declaration* EOF;

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

procedure: 'def' name=ID '(' (params+=variable (',' params+=variable)*)? ')' (':' returnType=type)? body=stmt;

crdtDecl: 'crdt' keyDecl;

variable: name=ID (':' type)?;

keyDecl: name=ID ':' crdttype;

type: name=ID ('[' typeArgs+=type (',' typeArgs+=type)* ']')?;

crdttype: 
      structcrdt
    | crdt
    ;

structcrdt: '{' keyDecl (',' keyDecl)* '}';

crdt: name=ID ('[' crdttype (','crdttype )* ']')?;

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

blockStmt: '{' stmt* '}';

assertStmt: 'assert' expr ;

atomicStmt: 'atomic' stmt;

localVar: 'var' variable ('=' expr)?;

ifStmt: 'if' '(' condition=expr ')' thenStmt=stmt ('else' elseStmt=stmt)?;

matchStmt: expr 'match' '{' cases+=matchCase* '}';

matchCase: 'case' expr '=>' stmt*;

crdtCall: 'call' functionCall;

assignment: varname=ID '=' expr;

newIdStmt: varname=ID '=' 'new' typename=ID;

returnStmt: 'return' expr (asserts+=assertStmt)*;

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
    | left=expr operator='==>' right=expr
    | quantifierExpr
    | functionCall
    | '(' parenExpr=expr ')'
    ;

quantifierExpr: quantifier=('forall'|'exists') vars+=variable (',' vars+=variable)* '::' expr;

functionCall: funcname=ID '(' (args+=expr (',' args+=expr)*)? ')';

invariant: free='free'? 'invariant' (name=ID ':')? expr;