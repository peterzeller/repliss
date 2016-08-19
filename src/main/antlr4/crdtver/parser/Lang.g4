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
    | invariant
;


typedecl: 'type' name=ID ('=' dataTypeCases+=dataTypeCase ('|' dataTypeCases+=dataTypeCase)*)?;

dataTypeCase: name=ID '(' (params+=variable (',' params+=variable)*)? ')';

operationDecl: 'operation' name=ID '(' (params+=variable (',' params+=variable)*)? ')';

queryDecl: 'query' name=ID '(' (params+=variable (',' params+=variable)*)? ')' ':' returnType=type '=' expr;

procedure: 'def' name=ID '(' (params+=variable (',' params+=variable)*)? ')' (':' returnType=type)? body=stmt;

variable: name=ID ':' type;

type: name=ID;

stmt:
      blockStmt
    | atomicStmt
    | localVar
    | ifStmt
    | crdtCall
    | assignment
    ;

blockStmt: '{' stmt* '}';

atomicStmt: 'atomic' stmt;

localVar: 'var' variable;

ifStmt: 'if' '(' condition=expr ')' thenStmt=stmt ('else' elseStmt=stmt)?;

crdtCall: 'call' functionCall;

assignment: varname=ID '=' expr;

expr:
      varname=ID
    | receiver=expr '.' fieldName=ID
    | left=expr 'is' isAttribute='visible'
    | left=expr 'happened' operator=('before'|'after') right=expr
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