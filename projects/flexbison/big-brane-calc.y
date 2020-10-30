%{
# include <stdio.h>
# include <stdlib.h>
# include "big-brane-calc.h"
%}

%union {
  struct ast *a;
  double d;
}

%token <d> NUMBER
%token EOL

%type <a> exp factor term

%%
calclist:
  | calclist exp EOL{
    printf("= %4.4g\n", eval($2));
    treefree($2);
    printf("> ");
    }
  | calclist EOL { printf("> "); }
  ;

/* finish the remaining parts of the grammar */
exp: 
%%
