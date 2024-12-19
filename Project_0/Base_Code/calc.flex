%{
#include "parser.h"
%}

%option nounput
%option noinput

%%
sin         return TOKEN_SIN;
cos         return TOKEN_COS;
[ \n\r\t]*		;
[0-9]*  	{yylval.value = atoi(yytext); return TOKEN_INT;}	
[a-zA-Z_]* 	{yylval.str = strdup(yytext);  return TOKEN_ID;}
\*			return TOKEN_MUL;
\+			return TOKEN_PLUS;
\-			return TOKEN_MINUS;
\/			return TOKEN_DIV;
\(			return TOKEN_LPAREN;
\)			return TOKEN_RPAREN;
\=    		return TOKEN_ASSIGN;
\! 			return TOKEN_ENDPROG;
\;			return TOKEN_SEMI;

.			{ printf("scan error: bad token: %c\n",yytext[0]); }
%%

/*
flex calls yywrap() whenever it reaches the end of the current file.
If yywrap returns false to indicate the end of the program.
It could alternatively open up another file and return true,
so that flex would keep going.
*/

int yywrap()
{
	return 0;
}


