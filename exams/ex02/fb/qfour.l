%{
# include "qfour.tab.h"
%}

%%
"+"     { return ADD; }
"-"     { return SUB; }
"*"     { return MUL; }
"/"     { return DIV; }
"|"     { return ABS; }
"("     { return OP; }
")"     { return CP; }
"//."   { /*ignore comments */}
[0-9]+  { yylval = atoi(yytext); return NUMBER; }
\n      { return EOL; }
[ \t]   { /*ignore whitespace */ }
.       { printf("Mystery Character %c\n", *yytext); }
%%
