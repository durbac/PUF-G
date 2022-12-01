%{
/*****************************************************************
 * Date of Creation: 17:50, Monday, May 4, 2020
 * File Name: puf_g.l
 * Purpose: This is a PUF G language lexical analyser intended to 
 			transform designs specified in PUF_G to PUFs for 
 			assessment of provable learnability 
 * Author: Durba Chatterjee
 * Revision History:
******************************************************************/             



/*****************************************************************
 * Header Include section
 *****************************************************************/
#include <stdio.h>
#include <string.h>
#include "y.tab.h"


/*****************************************************************
 * Macro Definitions
 *****************************************************************/

                                                                                
/*****************************************************************
 * Global Variable Declaration section
 *****************************************************************/

                                                                              
/*****************************************************************
 * Global Function Declaration section
 *****************************************************************/
// #undef ECHO
// #define ECHO printf ("[%s]\n", yytext)

%}

%%

[ \t] ;

begin_mod   {
			yylval.string = strdup(yytext);
            ECHO;
			return BEGIN_MOD;
		}

end_mod {
		yylval.string = strdup(yytext);
        ECHO;
		return END_MOD;
	}
    
return  {
            yylval.string = strdup(yytext);
            ECHO;
            return RETURN;
        }
    
if  {
        yylval.string = strdup(yytext);
        ECHO;
        return IF;
    }
    
then    {
            yylval.string = strdup(yytext);
            ECHO;
            return THEN;
        }
    
else    {
            yylval.string = strdup(yytext);
            ECHO;
            return ELSE;
        }
    
else_if {
            yylval.string = strdup(yytext);
            ECHO;
            return ELSE_IF;
        }
    
end_if  {
            yylval.string = strdup(yytext);
            ECHO;
            return END_IF;
        }
    
to  {
        yylval.string = strdup(yytext);
            ECHO;
        return TO;
    }

do  {
        yylval.string = strdup(yytext);
            ECHO;
        return DO;
    }

serial  {
            yylval.string = strdup(yytext);
            ECHO;
            return SERIAL;
        }

end_serial  {
                yylval.string = strdup(yytext);
                ECHO;
                return END_SERIAL;
            }

parallel    {
                yylval.string = strdup(yytext);
                ECHO;
                return PARALLEL;
            }

end_parallel    {
                    yylval.string = strdup(yytext);
                    ECHO;
                    return END_PARALLEL;
                }

num     {
            yylval.string = strdup(yytext);
            ECHO;
            return NUM;
        }

vec     {
            yylval.string = strdup(yytext);
            ECHO;
            return VEC;
        }

bit     {
            yylval.string = strdup(yytext);
            ECHO;
            return BIT;
        }


[&] {
     yylval.string = "&&";
     ECHO;
     return BOOL_AND;
    }

[|] {
     yylval.string = "||";
     ECHO;
     return BOOL_OR;
    }

[!] {
     yylval.string = strdup(yytext);
     ECHO;
     return BOOL_NOT;
    }

XOR {
     yylval.string = strdup(yytext);
     ECHO;
     return BOOL_XOR;
    }

[=][=] {
        yylval.string = strdup(yytext);
        ECHO;
        return EQUALITY;
       }

[!][=] {
        yylval.string = strdup(yytext);
        ECHO;
        return INEQUALITY;
       }

[>][=] {
        yylval.string = strdup(yytext);
        ECHO;
        return GREATER_THAN_EQUAL;
       }

[<][=] {
        yylval.string = strdup(yytext);
        ECHO;
        return LESSER_THAN_EQUAL;
       }

[>]    {
        yylval.string = strdup(yytext);
        ECHO;
        return GREATER_THAN;
       }

[<]    {
        yylval.string = strdup(yytext);
        ECHO;
        return LESSER_THAN;
       }

[+] {
     yylval.string = strdup(yytext);
     ECHO;
     return ADD;
    }

[-] {
     yylval.string = strdup(yytext);
     ECHO;
     return SUBTRACT;
    }

[*] {
     yylval.string = strdup(yytext);
     ECHO;
     return MULTIPLY;
    }

[/] {
     yylval.string = strdup(yytext);
     ECHO;
     return DIVIDE;
    }

[*][*] {
     yylval.string = strdup(yytext);
     ECHO;
     return POWER;
    }

[%] {
     yylval.string = strdup(yytext);
     ECHO;
     return MODULO;
    }

[=] {
     yylval.string = strdup(yytext);
     ECHO;
     return ASSIGN;
    }



[(] {
     yylval.string = strdup(yytext);
     ECHO;
     return OPEN_PARENTHESIS;
    }

[)] {
     yylval.string = strdup(yytext);
     ECHO;
     return CLOSE_PARENTHESIS;
    }

[{] {
     yylval.string = strdup(yytext);
     ECHO;
     return OPEN_BRACKET;
    }

[}] {
     yylval.string = strdup(yytext);
     ECHO;
     return CLOSE_BRACKET;
    }

[\[] {
      yylval.string = strdup(yytext);
      ECHO;
      return OPEN_BRACE;
     }

[\]] {
      yylval.string = strdup(yytext);
      ECHO;
      return CLOSE_BRACE;
     }

[,] {
     yylval.string = strdup(yytext);
     ECHO;
     return COMMA;
    }

[;] {
     yylval.string = strdup(yytext);
     ECHO;
     return SEMI_COLON;
    }

[:] {
     yylval.string = strdup(yytext);
    ECHO;
     return COLON;
    }

[a-zA-Z][a-zA-Z0-9_-]* {    //added [ ] for indexing in primitive call parameters
                        yylval.string = strdup(yytext);
                        ECHO;
                        return STRING;
                       }

[0-9][0-9]* {
        yylval.string = strdup(yytext);
        ECHO;
        return NUMBER;
       }

.|\n {ECHO; }

%%
