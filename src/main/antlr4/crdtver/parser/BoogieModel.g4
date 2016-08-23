grammar BoogieModel;

ID: [a-zA-Z_0-9$%+@!*,.|\[\]]+;

//ML_COMMENT: '/*' .*? '*/' -> skip;
//LINE_COMMENT: '//' ~[\r\n]* -> skip;
WS : [ \t]+ -> skip ;
NL: [\r\n]+ ;

STATE_START: '*** STATE' ~[\r\n]*;

model:
    '*** MODEL' NL
    variableValues+=variableValue*
    states+=state*
    '*** END_MODEL' NL;

variableValue:
    varName=ID '->' value=expr NL;

expr:
      varName=ID
    | '(' sExprValues+=expr* ')'
    | '{' NL tableEntry* '}'
    ;

tableEntry:
    keys+=expr+ '->' value=expr NL;

state:
    STATE_START NL
    variableValues+=variableValue*
    '*** END_STATE' NL
    ;