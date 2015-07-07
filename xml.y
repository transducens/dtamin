%token label
%token word
%token lpar
%token rpar

%{
#include <iostream>
#include <string>
int len;
typedef struct { std::string text; } data;
#define YYSTYPE data 
#include "pre.C"
int yyerror (char *s) { std::cerr << s << std::endl; return 0; }
%}


%%
X : lpar S rpar   
| X lpar S rpar         
  ;
S : lpar label L rpar  { if ( $3.text == "" ) 
                            std::cerr << "empty sentence" << std::endl;
                         $$.text = "<S1> <"+$2.text+"> " + $3.text + 
                                   " </"+$2.text + "> </S1>";
			 if ( len <= 40 ) {
			   std::cout << $$.text << std::endl; 
			 }
			 len=0;   }
  ;
L : L C        { $$.text = $1.text +
                    (($1.text.size()+$2.text.size())>0 ? " ":"") + $2.text; }
  | C          { $$.text = $1.text; }
  ;
C : lpar label L rpar {  if ( $3.text != "" )
                           $$.text = "<" + $2.text + "> "+
                                     $3.text + " </"+$2.text+">"; 
                         else $$.text = $3.text;
                      }
  | W                 { $$.text = $1.text; }
  ;
W : lpar label word rpar { 
               if ( $2.text != "NONE") {
                            ++len;
                            $$.text = "<" + $2.text + "/>";
                           } else 
			     $$.text = "";
               }
  ;
%%

int 
main() {
  len = 0;
  yyparse();
  return 0;
}




