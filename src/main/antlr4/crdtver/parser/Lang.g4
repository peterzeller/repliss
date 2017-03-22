grammar Lang;

ID: [a-zA-Z][a-zA-Z_0-9]*;

ML_COMMENT: '/*' .*? '*/' -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
WS : [ \t\r\n]+ -> skip ;

program: declaration*;

declaration:
      procedure
    | typedecl
    | operationDecl
    | queryDecl
    | axiomDecl
    | invariant
;


typedecl: kind=('idtype'|'type') name=ID ('=' dataTypeCases+=dataTypeCase ('|' dataTypeCases+=dataTypeCase)*)?;

dataTypeCase: name=ID '(' (params+=variable (',' params+=variable)*)? ')';

operationDecl: 'operation' name=ID '(' (params+=variable (',' params+=variable)*)? ')';

queryDecl: (inline='@inline')? 'query' name=ID '(' (params+=variable (',' params+=variable)*)? ')' ':' returnType=type
    ('=' implementation=expr | 'ensures' ensures=expr)?;

axiomDecl: 'axiom' expr;

procedure: 'def' name=ID '(' (params+=variable (',' params+=variable)*)? ')' (':' returnType=type)? body=stmt;

variable: name=ID ':' type;

type: name=ID;

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

localVar: 'var' variable;

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
    | receiver=expr '.' fieldName=ID
    | left=expr 'is' isAttribute='visible'
    | left=expr 'happened' operator=('before'|'after') right=expr
    | unaryOperator='!' right=expr
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

invariant: 'invariant' expr;