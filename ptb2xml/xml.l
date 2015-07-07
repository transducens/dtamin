text [^ ()\t\n\r]+
space [ \t\n\r]

%{
#include <cstring>
extern YYSTYPE yylval;
%}

%%

"("              { return(lpar); }
")"              { return(rpar); }
{text}{space}    { yylval.text =  
                   strtok(strdup(yytext),"-= \n\t\r");
                   return (label);
                 }
{text}           { 
                   yylval.text=strdup(yytext); 
                   return (word); 
                 }

{space}          /* eat up whitespace */
%%

int yywrap (void) { return 1; }










