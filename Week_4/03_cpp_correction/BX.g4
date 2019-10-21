grammar BX ;

program: varDecl* stmt* ;

varDecl: 'var' varInit (',' varInit)* ':' type ';' ;
varInit: VAR ('=' expr)? ;

type: 'int64' | 'bool' ;

stmt: VAR '=' expr ';'               # move
    | 'print' expr ';'               # print
    | block                          # stmtGroup
    | ifElse                         # if
    | whileLoop                      # while
    ;

ifElse: 'if' '(' expr ')' block ('else' ifCont)? ;
ifCont: ifElse                       # elseIf
      | block                        # else
      ;

whileLoop: 'while' '(' expr ')' block ;

block: '{' stmt* '}' ;

expr: VAR                                # variable
    | NUM                                # number
    | BOOL                               # bool
    | op=('~'|'-'|'!') expr              # unop
    | expr op=('*'|'/'|'%') expr         # multiplicative
    | expr op=('+'|'-') expr             # additive
    | expr op=('<<'|'>>') expr           # shift
    | expr op=('<'|'<='|'>'|'>=') expr   # inequation
    | expr op=('=='|'!=') expr           # equation
    | expr '&' expr                      # bitAnd
    | expr '^' expr                      # bitXor
    | expr '|' expr                      # bitOr
    | expr '&&' expr                     # logAnd
    | expr '||' expr                     # logOr
    | '(' expr ')'                       # parens
    ;

BOOL: 'true' | 'false' ;
VAR: [A-Za-z_][A-Za-z0-9_]* ;
NUM: [0-9]+ ;

LINECOMMENT: '//' ~[\r\n]* '\r'? '\n' -> skip;
WS: [ \t\r\n]+ -> skip ;