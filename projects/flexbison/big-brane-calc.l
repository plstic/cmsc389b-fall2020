%option noyywrap nodefault yylineno
%{
# include "big-brane-calc.h"
# include "big-brane-calc.tab.h"
%}

%%

/* put the regex and what should be returned here */


/* when doing floats, do this, instead of yylval = yytext */
/* you will need to modify this line to look more like the thing in simp-calc */

"float/number regex" { yylval.d = yytext; }
%%

