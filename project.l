%{
#include <stdio.h>
#include <stdlib.h>
extern int yylineno;
extern YYSTYPE yylval;
YYSTYPE yylval;

%}
%%
\:		return COLUMS;
"&&"      	return AND;
"||"    	return OR; 
!=     	 	return ROP_NE;
==      	return ROP_EQ;
>       	return ROP_GT;
>=      	return ROP_GE;
\<      	return ROP_LT;
\<=     	return ROP_LE;
\!       	return NOT;
\=       	return ASSIGN;
\+      	return OP_PLUS;
\-      	return OP_MINUS;
\*     	 	return OP_MULT;
\/      	return OP_DIV;
\&      	return ADRS;
\^      	return PTR_VAL;
\;		return SEMI;
\,		return COMMA;
\{		return CURLY_O;
\}		return CURLY_C;
\(		return PAREN_O;
\)		return PAREN_C;
\|      	return BAR;
\[		return SQR_O;
\]		return SQR_C;
var|VAR   	return VAR;
else      	return ELSE;
if        	return IF;
while      	return WHILE;
for        	return FOR;
function	return FUNCTION;
void		return VOID_TYPE;
bool   		return BOOL_TYPE;
int    		return INT_TYPE;
real   		return REAL_TYPE;
char   		return CHAR_TYPE;
string   	return STRING_TYPE;
int\*   	return INTP_TYPE;
real\* 		return REALP_TYPE;
char\*   	return CHARP_TYPE;
return    	return RETURN;
"null"|"NULL"	return NULL_;
do		return DO;
main		return MAIN;



("true"|"false")							{yylval.string=strdup(yytext); return BOOL;}
[a-zA-Z][a-zA-Z0-9]*([_][a-zA-Z]+)?					{yylval.string=strdup(yytext); return ID;}
'[a-zA-Z0-9~`!@#\$%\^&\*\(\)_\-\+={\[\}\]\|\\:;"'<,>\.\?\/  ]?'		{yylval.string=strdup(yytext); return CHAR;}
[0-9]+|0[xX][A-Fa-f0-9]+						{yylval.string=strdup(yytext); return INT;}
[+-]?[0-9]*\.?[0-9]+([eE][+-]?[0-9]+)?					{yylval.string=strdup(yytext); return REAL;}
\"[a-zA-Z0-9~`!@#\$%\^&\*\(\)_\-\+={\[\}\]\|\\:;'<,>\.\?\/  ]*\"	{yylval.string=strdup(yytext); return STRING;}

[/][*][^*]*[*]+([^*/][^*]*[*]+)*[/] 
"//".*	
[ \t]	
[ \n]									{yylineno++;}	
.						return yytext[0];
%%
