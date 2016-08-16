grammar Lang;

ID: [a-zA-Z][a-zA-Z_0-9]*;

ML_COMMENT: '/*' .*? '*/' -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
WS : [ \t\r\n]+ -> skip ;

program: declaration*;

declaration:
      procedure
    | typedecl
    | invariant
;


typedecl: 'type' name=ID;

procedure: 'def' name=ID '(' (params+=variable (',' params+=variable)*)? ')' (':' returnType=type)? body=stmt;

variable: name=ID ':' type;

type: name=ID;

stmt:
      blockStmt
    | atomicStmt
    | localVar
    | ifStmt
    | crdtQuery
    | crdtCall
    ;

blockStmt: '{' stmt* '}';

atomicStmt: 'atomic' stmt;

localVar: 'var' variable;

ifStmt: 'if' '(' condition=expr ')' thenStmt=stmt ('else' elseStmt=stmt)?;

crdtQuery: varname=ID '=' funcname=ID '(' (args+=expr (',' args+=expr)*)? ')';

crdtCall: 'call' functionCall;

expr:
      varname=ID
    | left=expr operator=('<'|'<='|'>'|'>=') right=expr
    | left=expr operator=('=='|'!=') right=expr
    | left=expr operator='&&' right=expr
    | left=expr operator='||' right=expr
    | left=expr operator='==>' right=expr
    | quantifierExpr
    | functionCall
    | '(' expr ')'
    ;

quantifierExpr: 'forall' (vars+=variable) '::' expr;

functionCall: funcname=ID '(' (args+=expr (',' args+=expr)*)? ')';

invariant: 'invariant' expr;