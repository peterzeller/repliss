grammar Lang;

ID: [a-zA-Z][a-zA-Z_0-9]*;

ML_COMMENT: '/*' .*? '*/' -> skip;
LINE_COMMENT: '//' ~[\r\n]* -> skip;
WS : [ \t\r\n]+ -> skip ;

program: declaration*;

declaration:
    procedure
;


procedure: 'def' name=ID '(' (params+=parameter (',' params+=parameter)*)? ')' (':' returnType=type)? body=stmt;

parameter: name=ID ':' returnType=type;

type: name=ID;

stmt:
      blockStmt
    | atomicStmt
    | ifStmt
    | crdtQuery
    | crdtCall
    ;

blockStmt: '{' stmt* '}';

atomicStmt: 'atomic' stmt;

ifStmt: 'if' '(' condition=expr ')' thenStmt=stmt ('else' elseStmt=stmt)?;

crdtQuery: varname=ID '=' funcname=ID '(' (args+=expr (',' args+=expr)*)? ')';

crdtCall: 'call' funcname=ID '(' (args+=expr (',' args+=expr)*)? ')';

expr:
    varname=ID
    ;

