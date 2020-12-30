%{
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

int yyerror();
extern int yylex();
extern int yylineno;
extern char *yytext;

typedef struct node {
	char *token;
	struct node *left;
	struct node *right;
	char *code;
	char *var;
	char *label;
	char *truelabel;
	char *falselabel;
	int sum;
}node;

typedef struct Arguments {
    char **name;
    char **type;
    char **len;
}Arguments;

typedef struct Function {
    char *name;
    struct Arguments *args;
    char *return_type;
    int count_args;
}Function;

typedef struct Varaiables {
    int isArg;
    char **name;
    char **value;
    char **type;
    char **len;
}Varaiable;

typedef struct code_scope {
    int scope_level;
    char *name;
    Varaiable *var;
    Function *func;
    int countvar;
    int countfunc;
    struct code_scope *next_scope;
    struct code_scope *previous_scope;
}code_scope;

code_scope *stack = NULL;
static int current_scope = 0;


struct node* mkleaf(char *token);
struct node* mknode2(char *token,node *left, node *right);
int printtree(node* tree);
void syntaxMKscope(node *tree);
code_scope* mkcode(char* name);
void push(code_scope *new_scope); // ADDS NEW SCOPE TO THE STACK //
Arguments* mkargs(node *tree, int *args_count); // CREATES ARGUMENT SCOPE //
Function* create_function(char *name, Arguments *args, char *return_type, int countArgs, code_scope *code_scope); // CREATES FUNCTION SCOPE //
code_scope* create_scope(char* name, Function* function); // CREATES NEW SCOPE //
void add_varaiable(node* tree); // ADDS VARAIABLES TO CURRENT SCOPE //
void add_string_varaiable(node* tree);
void add_varaiable_rec(node* tree, char* var_type);
char* insert_values_into_vars(node* tree, char* var_type);
bool var_redeclaration_check(char* token);
char* var_declaration_check(char* token, int scope_level);
void func_existence_check(char* token);
char* func_call_existence_check(char* token, int scope_level, node* tree);
void func_call_args_check(code_scope *scope, node* tree);
void func_call_args_type_check(code_scope *scope, int index, char* type , char* token);
void return_check(node* tree, char* var_type);
void boolean_statement_check(node* tree);
char* arithmetic_statement_check(node* tree);
void for_initialization_check(node* tree);
void for_condition_check(node* tree);
void assignment(node* tree);

//part 3
static node * firstNode;
int POPParams(Arguments * args,int count);
void addCode(node* node,char *code,char *var,char *label,char *truelabel,char *falselabel);
void addCode2(node* node,char *code,char *var,char *label,char *truelabel,char *falselabel);
char* freshVar();
char* freshLabel();
char* gen(char*,char*,char*,char*,char*);
char* mystrcat(char*des,char*src);
char* mystrcat2(char*des,char*src);
char *replaceWord(const char *s, const char *oldW, const char *newW);
char * mkspace(char *label);
void calc3AC(node * tree);
int findfunc(char *func_name);
static int t = 1;
static int l = 1;
static int line = 1;


%}
%union {
  char* string; 
  struct node* node;
}

%token <string> INT CHAR STRING REAL BOOL ID VERTICAL_BAR
%token <string> REAL_TYPE INT_TYPE CHAR_TYPE STRING_TYPE REALP_TYPE CHARP_TYPE INTP_TYPE VOID_TYPE BOOL_TYPE
%token <string> RETURN NULL_ DO FUNCTION MAIN FOR WHILE IF ELSE VAR SQR_C SQR_O BAR PAREN_C PAREN_O CURLY_C CURLY_O
%token <string> COMMA SEMI PTR_VAL ADRS OP_DIV OP_MULT OP_MINUS OP_PLUS OP_PLUS_PLUS ASSIGN NOT COLUMS
%left  OR AND ROP_NE ROP_EQ ROP_GE ROP_LT ROP_LE ROP_GT
%left  OP_PLUS OP_MINUS OP_MULT OP_DIV
%right NOT ADRS PTR_VAL
%type <node> program functions function main_func code fargs body_f body_p f_args stmt_f stmt_p func_args code_ assignment expr func_call id_list var_decleration vardec_args array_id bool_expr bool_expr_
%type <node> stmt_block_rec decleration block_stmt code_block_dec ptr_expr
%type <node> code_block stmts_oneline procedure return for_init fbody_ pbody_ end_return str_decleration str_dec_args ptr_id
%type <string> id args_type f_type literal char_or_string int_or_id
%nonassoc ELSE
%nonassoc ID
%nonassoc PAREN_O
%nonassoc CURLY_O

%start program
%%
program		    :	code { printtree($1); printf("\n"); syntaxMKscope($1); printf("\n"); /*calc3AC($1);*/ }
                ;

code            :   code_ { $$ = mknode2("CODE", NULL, $1); }
                ;

code_		    :	function functions { $$ = mknode2("FUNCTION", $1, $2); }
                | 	procedure functions { $$ = mknode2("PROCEDURE", $1, $2); }
                |	main_func { $$ = mknode2("FUNCTION",$1,NULL); }
                ;
    
functions	    :   code_ { $$ = $1; }
                ;

function	    :	FUNCTION f_type id PAREN_O func_args PAREN_C CURLY_O body_f CURLY_C
                    { $$ = mknode2($3, mknode2($2, $5, NULL), $8); }
                ;

procedure       :	FUNCTION VOID_TYPE id PAREN_O func_args PAREN_C CURLY_O body_p CURLY_C 
                    { $$ = mknode2($3, mknode2("VOID TYPE",$5, NULL), $8);}
                ;

main_func	    :   FUNCTION VOID_TYPE MAIN PAREN_O PAREN_C CURLY_O body_p CURLY_C
                    { $$ = mknode2("MAIN",mknode2("VOID TYPE", mkleaf("ARGS NONE"),NULL), $7); }
                ;

args_type	    :	REAL_TYPE { $$ = strdup("REAL"); }
                |  	BOOL_TYPE { $$ = strdup("BOOL"); }
                |	INT_TYPE { $$ = strdup("INT"); }
                | 	CHAR_TYPE { $$ = strdup("CHAR"); }
                | 	INTP_TYPE { $$ = strdup("INTP"); }
                | 	CHARP_TYPE { $$ = strdup("CHARP"); }
                | 	STRING_TYPE { $$ = strdup("STRING"); }
                ;

f_type		    :	BOOL_TYPE { $$ = strdup("BOOL"); }
                | 	INT_TYPE { $$ = strdup("INT"); }
                | 	CHAR_TYPE { $$ = strdup("CHAR"); }
                | 	INTP_TYPE { $$ = strdup("INTP"); }
                | 	CHARP_TYPE { $$ = strdup("CHARP"); }
                |   REAL_TYPE { $$ = strdup("REAL"); }
                ;   

id		        :	ID { $$ = strdup(yytext); }
                ;

array_id	    :	id SQR_O INT SQR_C {$$=mknode2($1,mkleaf($3),NULL);}
                | 	id SQR_O id SQR_C {$$=mknode2($1,mkleaf($3),NULL);}
                ;

func_args       :   f_args { $$ = mknode2("ARGS", $1, NULL); }
                |   { $$ = mknode2("ARGS NONE", NULL, NULL); }
                ;

f_args		    :   args_type fargs SEMI f_args { $$ = mknode2($1, $4, $2); }
                |   args_type fargs { $$ = mknode2($1, NULL, $2); }
                ;

fargs		    :   id COMMA fargs { $$ = mknode2($1, NULL, $3); }
                |   id { $$ = mkleaf($1); }
                ;


body_f	    	:  	fbody_ end_return { $$ = mknode2("BODY", $1, $2); }
                ;

body_p	    	:  	pbody_ { $$ = mknode2("BODY", $1, NULL); }
                ;

fbody_          :   stmt_f fbody_ { $$ = mknode2("STATEMENT", $1, $2); }
               	|   { $$ = NULL; }
                ;

pbody_          :   stmt_p pbody_ { $$ = mknode2("STATEMENT", $1, $2); }
                |   { $$ = NULL; }
                ;

assignment 	    :   id ASSIGN expr { $$ = mknode2("=", mkleaf($1), $3); }
                |	array_id ASSIGN expr { $$ = mknode2("=", $1, $3); }
                |   ptr_id ASSIGN expr { $$ = mknode2("=", $1, $3); }
                ;

ptr_id          :   OP_MULT id { $$=mknode2("PTR",mkleaf($2),NULL); }
                ;

for_init        :	assignment SEMI expr SEMI assignment {
                    $$ = mknode2("FOR-INIT",$1, mknode2("FOR-CONDITION", $3, $5));
                    addCode($3, NULL, NULL, NULL, freshLabel(), NULL);
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); }
                ;

stmts_oneline   : 	assignment SEMI { $$ = mknode2("STATEMENT", mknode2("ASSIGNMENT", $1, NULL), NULL); }
                ;

stmt_p       	:   IF PAREN_O bool_expr PAREN_C CURLY_O pbody_ CURLY_C %prec ELSE {
                    $$=mknode2("IF", $3, mknode2("IF-BODY",$6, NULL));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); }
                |   IF PAREN_O bool_expr PAREN_C CURLY_O pbody_ CURLY_C ELSE CURLY_O pbody_ CURLY_C {
                    $$ = mknode2("IF-ELSE", mknode2("IF", $3, mknode2("IF-BODY", $6, NULL)), mknode2("ELSE", mknode2("ELSE-BODY", $10, NULL), NULL));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); addCode($3, NULL, NULL, NULL, freshLabel(), NULL); }
                |   IF PAREN_O bool_expr PAREN_C stmts_oneline %prec ELSE {
                    $$ = mknode2("IF", $3, mknode2("IF-BODY", $5, NULL));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); }
                |   IF PAREN_O bool_expr PAREN_C stmts_oneline ELSE stmts_oneline {
                    $$ = mknode2("IF-ELSE", mknode2("IF", $3, mknode2("IF-BODY", $5, NULL)), mknode2("ELSE", mknode2("ELSE-BODY", $7, NULL),NULL));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); addCode($3, NULL, NULL, NULL, freshLabel(), NULL); }
                |   IF PAREN_O bool_expr PAREN_C stmts_oneline ELSE CURLY_O stmts_oneline CURLY_C
                    { $$ = mknode2("IF-ELSE", mknode2("IF", $3, $5), mknode2("ELSE", $8, NULL));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); addCode($3, NULL, NULL, NULL, freshLabel(), NULL); }
                | 	WHILE PAREN_O bool_expr PAREN_C CURLY_O  pbody_ CURLY_C {
                    $$ = mknode2("WHILE", $3, mknode2("WHILE-BODY",$6, NULL));
                    addCode($3, NULL, NULL, NULL, freshLabel(), NULL); addCode($3, NULL, NULL, NULL, NULL, freshLabel());  }
                |	WHILE PAREN_O bool_expr PAREN_C stmts_oneline SEMI {
                    $$ = mknode2("WHILE", $3, mknode2("WHILE-BODY", $5, NULL));
                    addCode($3, NULL, NULL, NULL, freshLabel(), NULL); addCode($3, NULL, NULL, NULL, NULL, freshLabel()); }
                |	DO CURLY_O pbody_ CURLY_C WHILE PAREN_O bool_expr PAREN_C SEMI {
                    $$ = mknode2("DO-WHILE", mknode2("DO-WHILE-BODY",$3,NULL), $7);
                    addCode($7, NULL, NULL, NULL, freshLabel(), NULL); addCode($7, NULL, NULL, NULL, NULL, freshLabel()); }
                | 	FOR PAREN_O for_init PAREN_C CURLY_O pbody_ CURLY_C { $$ = mknode2("FOR",$3, mknode2("FOR-BODY",$6, NULL)); }
                |	FOR PAREN_O for_init PAREN_C stmts_oneline SEMI { $$ = mknode2("FOR",$3, mknode2("FOR-BODY",$5, NULL)); }
                |	decleration { $$ = mknode2("DECLETARION", $1, NULL); }
                |	func_call SEMI { $$ = $1; }
                | 	assignment SEMI  { $$ = mknode2("ASSIGNMENT", $1, NULL); }
                |	procedure { $$ = mknode2("PROCEDURE", $1, NULL); }
                |	function { $$ = mknode2("FUNCTION", $1, NULL); }
                |	CURLY_O code_block CURLY_C { $$ = $2; }
                ;

stmt_f       	:   IF PAREN_O bool_expr PAREN_C CURLY_O fbody_ return CURLY_C %prec ELSE {
                    $$=mknode2("IF", $3, mknode2("IF-BODY",$6, $7));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); }
                |   IF PAREN_O bool_expr PAREN_C CURLY_O fbody_ return CURLY_C ELSE CURLY_O fbody_ return CURLY_C {
                    $$ = mknode2("IF-ELSE", mknode2("IF", $3, mknode2("IF-BODY",$6,$7)), mknode2("ELSE", mknode2("ELSE-BODY",$11,$12), NULL));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); addCode($3, NULL, NULL, NULL, freshLabel(), NULL); }
                |   IF PAREN_O bool_expr PAREN_C stmts_oneline %prec ELSE {
                    $$ = mknode2("IF", $3, mknode2("IF-BODY", $5, NULL));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); }
                |   IF PAREN_O bool_expr PAREN_C stmts_oneline ELSE stmts_oneline {
                    $$ = mknode2("IF-ELSE", mknode2("IF", $3, mknode2("IF-BODY", $5, NULL)), mknode2("ELSE", mknode2("ELSE-BODY", $7, NULL),NULL));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); addCode($3, NULL, NULL, NULL, freshLabel(), NULL); }
                |   IF PAREN_O bool_expr PAREN_C stmts_oneline ELSE CURLY_O stmts_oneline CURLY_C {
                    $$ = mknode2("IF-ELSE", mknode2("IF", $3, mknode2("IF-BODY", $5, NULL)), mknode2("ELSE", mknode2("ELSE-BODY", $8, NULL), NULL));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); addCode($3, NULL, NULL, NULL, freshLabel(), NULL); }
                | 	WHILE PAREN_O bool_expr PAREN_C CURLY_O  fbody_ return CURLY_C {
                    $$ = mknode2("WHILE", $3, mknode2("WHILE-BODY",$6,$7));
                    addCode($3, NULL, NULL, NULL, freshLabel(), NULL); addCode($3, NULL, NULL, NULL, NULL, freshLabel());  }
                |	WHILE PAREN_O bool_expr PAREN_C stmts_oneline SEMI {
                    $$ = mknode2("WHILE", $3, mknode2("WHILE-BODY", $5, NULL));
                    addCode($3, NULL, NULL, NULL, freshLabel(), NULL); addCode($3, NULL, NULL, NULL, NULL, freshLabel()); }
                |	DO CURLY_O fbody_ return CURLY_C WHILE PAREN_O bool_expr PAREN_C SEMI {
                    $$ = mknode2("DO-WHILE", mknode2("DO-WHILE-BODY", $3, $4), $8);
                    addCode($8, NULL, NULL, NULL, freshLabel(), NULL); addCode($8, NULL, NULL, NULL, NULL, freshLabel()); }
                | 	FOR PAREN_O for_init PAREN_C CURLY_O fbody_ return CURLY_C { $$ = mknode2("FOR",$3,mknode2("FOR-BODY", $6, $7)); }
                |	FOR PAREN_O for_init PAREN_C stmts_oneline SEMI { $$ = mknode2("FOR", $3, mknode2("FOR-BODY",$5, NULL));}
                |	decleration { $$ = mknode2("DECLETARION", $1, NULL); }
                |	func_call SEMI { $$ = $1; }
                | 	assignment SEMI { $$ = mknode2("ASSIGNMENT", $1, NULL); }
                |	procedure { $$ = mknode2("PROCEDURE", $1, NULL); }
                |	function { $$ = mknode2("FUNCTION", $1, NULL); }
                |	CURLY_O code_block CURLY_C { $$ = $2; }
                ;

block_stmt      :   IF PAREN_O bool_expr PAREN_C CURLY_O stmt_block_rec CURLY_C %prec ELSE  {
                    $$=mknode2("IF", $3, mknode2("IF-BODY", $6, NULL));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); }
                |   IF PAREN_O bool_expr PAREN_C CURLY_O stmt_block_rec CURLY_C ELSE CURLY_O pbody_ CURLY_C {
                    $$ = mknode2("IF-ELSE", mknode2("IF", $3, mknode2("IF-BODY", $6, NULL)), mknode2("ELSE", mknode2("ELSE-BODY", $10, NULL), NULL));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); addCode($3, NULL, NULL, NULL, freshLabel(), NULL); }
                |   IF PAREN_O bool_expr PAREN_C stmts_oneline %prec ELSE {
                    $$ = mknode2("IF", $3, mknode2("IF-BODY", $5, NULL));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); }
                |   IF PAREN_O bool_expr PAREN_C stmts_oneline ELSE stmts_oneline {
                    $$ = mknode2("IF-ELSE", mknode2("IF", $3, mknode2("IF-BODY", $5, NULL)), mknode2("ELSE", mknode2("ELSE-BODY", $7, NULL),NULL));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); addCode($3, NULL, NULL, NULL, freshLabel(), NULL); }
                |   IF PAREN_O bool_expr PAREN_C stmts_oneline ELSE CURLY_O stmts_oneline CURLY_C {
                    $$ = mknode2("IF-ELSE", mknode2("IF", $3, mknode2("IF-BODY", $5, NULL)), mknode2("ELSE", mknode2("ELSE-BODY", $8, NULL), NULL));
                    addCode($3, NULL, NULL, NULL, NULL, freshLabel()); addCode($3, NULL, NULL, NULL, freshLabel(), NULL); }
                |   WHILE PAREN_O bool_expr PAREN_C CURLY_O stmt_block_rec CURLY_C {
                    $$ = mknode2("WHILE", $3, mknode2("WHILE-BODY",$6, NULL));
                    addCode($3, NULL, NULL, NULL, freshLabel(), NULL); addCode($3, NULL, NULL, NULL, NULL, freshLabel()); }
                |   WHILE PAREN_O bool_expr PAREN_C stmts_oneline SEMI {
                    $$ = mknode2("WHILE", $3, mknode2("WHILE-BODY",$5, NULL));
                    addCode($3, NULL, NULL, NULL, freshLabel(), NULL); addCode($3, NULL, NULL, NULL, NULL, freshLabel()); }
                |   DO CURLY_O stmt_block_rec CURLY_C WHILE PAREN_O bool_expr PAREN_C SEMI {
                    $$ = mknode2("DO-WHILE", mknode2("DO-WHILE-BODY",$3,NULL), $7);
                    addCode($7, NULL, NULL, NULL, freshLabel(), NULL); addCode($7, NULL, NULL, NULL, NULL, freshLabel()); }
                |   FOR PAREN_O for_init PAREN_C CURLY_O stmt_block_rec CURLY_C {$$=mknode2("FOR",$3, mknode2("FOR-BODY",$6, NULL));}
                |   FOR PAREN_O for_init PAREN_C stmts_oneline SEMI {$$=mknode2("FOR",$3, mknode2("FOR-BODY",$5, NULL));}
                |   func_call SEMI { $$ = $1; }
                |   assignment SEMI { $$ = mknode2("ASSIGNMENT", $1, NULL); }
                |   CURLY_O code_block CURLY_C { $$ = $2; }
                ;

decleration     :   var_decleration { $$ = $1; } | STRING_TYPE str_decleration SEMI { $$ = mknode2("STRING", $2, NULL); }
                ;

var_decleration :	VAR args_type vardec_args SEMI { $$ = mknode2($2, $3, NULL); }
                ;

vardec_args     :	id { $$ = mkleaf($1); }
                |	id COMMA vardec_args { $$ = mknode2($1, $3,NULL); }
                |	assignment COMMA vardec_args { $$ = mknode2("ASSIGN-DECLER", $1, $3); }
                |	assignment { $$ = mknode2("ASSIGN-DECLER", $1, NULL); }
                ;

str_decleration :   str_dec_args { $$ = $1; }
                ;

str_dec_args    :   id SQR_O int_or_id SQR_C COMMA str_dec_args { $$ = mknode2($1, $6 , mkleaf($3));}
                |   id SQR_O int_or_id SQR_C { $$ = mknode2($1, NULL, mkleaf($3)); }
                |   id SQR_O int_or_id SQR_C ASSIGN char_or_string { $$ = mknode2($1, NULL, mknode2($3, NULL, mkleaf($6))); }
                |   id SQR_O int_or_id SQR_C ASSIGN char_or_string COMMA str_dec_args { $$ = mknode2($1, $8, mknode2($3, NULL, mkleaf($6))); }
                ;

int_or_id       :   INT {$$ = strdup(yytext); } | id
                ;

char_or_string  :   CHAR { $$ = strdup(yytext); }
                | 	STRING { $$ = strdup(yytext); }
                ;

func_call       :	id PAREN_O id_list PAREN_C {$$=mknode2("FUNC CALL",mkleaf($2),$3);}
                ;

id_list         :	id COMMA id_list {$$=mknode2($1,$3,NULL);} 
                | 	id {$$=mkleaf($1);}
                | 	literal COMMA id_list {$$=mknode2($1,$3,NULL);}
                |	literal { $$=mkleaf($1); }
                | 	{$$=NULL;}
                ;

bool_expr       :   expr OR bool_expr_ {
                    $$ = mknode2("||", $3, $1); addCode($1, NULL, NULL, NULL,NULL ,freshLabel()); addCode($1, NULL, NULL, NULL,freshLabel() ,NULL); }
                |   expr AND bool_expr_ { $$ = mknode2("&&", $3, $1); }
                |   expr { $$ = $1; }
                ;

bool_expr_      :   expr OR bool_expr_ { $$ = mknode2("||", $3, $1); addCode($1, NULL, NULL, NULL, freshLabel(), freshLabel()); }
                |   expr AND bool_expr_ { $$ = mknode2("&&", $3, $1); }
                |   expr { $$ = $1; }
                ;

expr     	    :   expr OP_PLUS expr { $$ = mknode2("+", $1, $3); }
                |   expr OP_MINUS expr { $$ = mknode2("-", $1, $3); }
                |   expr OP_MULT expr  { $$ = mknode2("*", $1, $3); }
                |   expr OP_DIV expr { $$ = mknode2("/", $1, $3); }
                |   expr ROP_NE expr { $$ = mknode2("!=", $1, $3); }
                |   expr ROP_EQ expr { $$ = mknode2("==", $1, $3); }
                |   expr ROP_GT expr { $$ = mknode2(">", $1, $3); }
                |   expr ROP_GE expr { $$ = mknode2(">", $1, $3); }
                |   expr ROP_LT expr { $$ = mknode2("<", $1, $3); }
                |   expr ROP_LE expr { $$ = mknode2("<=", $1, $3); }
                |   NOT expr { $$ = mknode2("!", NULL, $2); }
                |   OP_MULT ptr_expr { $$ = mknode2("PTR",$2, NULL); }
                |  	ADRS ptr_expr{ $$ = mknode2("&", NULL, $2); }
                |   literal { $$ = mkleaf($1); }
                |   func_call { $$ = $1; }
                |   BAR id BAR { $$ = mknode2("|", mkleaf($2), NULL); }
                |   BAR STRING BAR { $$ = mknode2("|", mkleaf($2), NULL); }
                |   id SQR_O int_or_id SQR_C { $$ = mknode2($1, mkleaf($3), NULL); }
                |   id	{ $$ = mkleaf($1); }
                |   PAREN_O expr PAREN_C { $$ = mknode2("()", NULL, $2); }
                ;

ptr_expr	    :	PAREN_O id OP_PLUS INT PAREN_C  { $$ = mknode2("+", mkleaf($2),mkleaf($4)); }
                |	PAREN_O id OP_MINUS INT PAREN_C  { $$ = mknode2("+", mkleaf($2),mkleaf($4)); }
                |	PAREN_O id OP_MULT INT PAREN_C  { $$ = mknode2("+", mkleaf($2),mkleaf($4)); }
                |	PAREN_O id OP_DIV INT PAREN_C  { $$ = mknode2("+", mkleaf($2),mkleaf($4)); }
                | 	id { $$ = mknode2($1, NULL,NULL); }
                | 	array_id { $$ = $1; }
                ;

code_block      :   code_block_dec stmt_block_rec { $$ = mknode2("CODE BLOCK", $1, $2); }
                ;

stmt_block_rec  :   block_stmt stmt_block_rec { $$ = mknode2("STATEMENT", $1, $2); } | { $$ = NULL; }
                ;

code_block_dec  :   decleration code_block_dec { $$ = mknode2("STATEMENT", mknode2("DECLETARION", $1, NULL), $2); } | { $$ = NULL; }
                ;

literal    	    : 	INT { $$ = strdup(yytext); }
                | 	CHAR { $$ = strdup(yytext); }
                | 	STRING { $$ = strdup(yytext); }
                | 	REAL { $$ = strdup(yytext); }
                | 	NULL_ { $$ = strdup(yytext); }
                | 	BOOL { $$ = strdup(yytext); }
                ;

return          :	RETURN expr SEMI { $$ = mknode2("RETURN",$2,NULL); }
                |	{ $$=NULL; }
                ;

end_return      :	RETURN expr SEMI { $$ = mknode2("RETURN",$2,NULL); }
                ;

%%
#include "lex.yy.c"
int main() {
	yyparse();
	return 0;
}

int tabCount1 = 0;
int tabCount2 = 0;

struct node* mkleaf(char* token) {
	node* newnode=(node*)malloc(sizeof(node));
	char* newstr;

	if(token) {
		newstr=(char*)malloc(sizeof(token)+1);
		newstr[sizeof(token)]='\0';
		strcpy(newstr,token);
	}
	else {
        newstr=(char*)malloc(1);
		strcpy(newstr,"");
		
	}
    newnode->left=NULL;
	newnode->right=NULL;
	newnode->token=newstr;
	return newnode;
}


struct node* mknode2(char* token,node*left,node*right) {
    node* newnode = mkleaf(token);
	newnode->left=left;
	newnode->right=right;
    return newnode;
}

void printTabs1() {
	for(int i=0;i<tabCount1;i++){
		printf(".  ");	
	}
}

void printTabs2() {
	int i;
	for(i=0;i<tabCount2;i++) {
		printf(".  ");
	}
}

void openTag() {
	printf("(%d)" , tabCount1);	
}

void closeTag() {
	printf("(/%d)" , tabCount1);	
}

int printtree(node* tree) {
	tabCount1++;
    if (tree->left!=NULL || tree->right!=NULL){
        if(strcmp(tree->token," ")!=0){
        printTabs1();
        printf("(");
        printf("%s\n",tree->token);
        tabCount2=tabCount1;
        }
    }
    else if(tree->left==NULL && tree->right==NULL){
        printTabs1();
        printf("(");
        printf("%s",tree->token);
        printf(")\n");
        }
    
    if(tree->left){
        printf("LEFT: ");
        printtree(tree->left);
    }
    if(tree->right)
    {
        printf("RIGHT: ");
        printtree(tree->right);
    }
    if(tree->left != NULL || tree->right != NULL){
        if(strcmp(tree->token," ") !=0 ) {
            printTabs2();
            printf(")\n");
            tabCount2--;
        }
    }
    tabCount1--;
    return 1;

}

void syntaxMKscope(node *tree) {
    if (tree == NULL) { return;/*FINISH*/}
    else if (!strcmp(tree->token, "FUNCTION") || !strcmp(tree->token, "PROCEDURE")) {
        current_scope++; // JUMP TO NEXT SCOPE LEVEL //
        int args_count = 0;
        Function *function = NULL;
        Arguments *function_args = NULL;
        if (strcmp(tree->left->left->left->token, "ARGS NONE")) {
            function_args = mkargs(tree->left->left->left->left, &args_count);
        }
        // FUNCTION CREATION //
        function = create_function(/*FUNCTION NAME*/ tree->left->token, function_args,/*FUNCTION TYPE*/ tree->left->left->token,/*ARGS COUNT*/ args_count, NULL);
        // CHECKS IF THE FUNCTION ALREADY EXIST IN THE CURRENT SCOPE //
        func_existence_check(tree->left->token);
        // SCOPE CREATION //
        code_scope *new_scope = create_scope("FUNCTION", function);
        push(new_scope);
        syntaxMKscope(tree->left->right->left); // MOVING INTO THE BODY //
        current_scope--; // JUMP TO PREVIOUS SCOPE LEVEL //
        syntaxMKscope(tree->left->right->right); // MOVING INTO THE BODY //
        syntaxMKscope(tree->right); // MOVE TO NEXT FUNCTION //
    }
    
    else if (!strcmp(tree->token, "CODE")) {
        push(NULL);
        syntaxMKscope(tree->right);
    }
    
    else if (!strcmp(tree->token, "STATEMENT")) {
        syntaxMKscope(tree->left);
        syntaxMKscope(tree->right);
    }
    
    else if (!strcmp(tree->token, "DECLETARION")) {
        add_varaiable(tree->left);
    }
    
    else if (!strcmp(tree->token, "FUNC CALL")) {
        current_scope++;
        func_call_existence_check(tree->left->token, current_scope, tree);
        current_scope--;
    }
    
    else if (!strcmp(tree->token, "IF-ELSE")) {
        syntaxMKscope(tree->left); // IF //
        syntaxMKscope(tree->right); // ELSE //
    }
    
    else if (!strcmp(tree->token, "WHILE")) {
        boolean_statement_check(tree->left); // IF VARS //
        syntaxMKscope(tree->right);
    }
    
    else if (!strcmp(tree->token, "WHILE-BODY")) {
        current_scope++;
        code_scope *new_scope = create_scope("WHILE", NULL);
        push(new_scope);
        syntaxMKscope(tree->left);
        syntaxMKscope(tree->right);
        current_scope--;
    }
    
    else if (!strcmp(tree->token, "DO-WHILE")) {
        boolean_statement_check(tree->right); // IF VARS //
        syntaxMKscope(tree->left);
    }
    
    else if (!strcmp(tree->token, "DO-WHILE-BODY")) {
        current_scope++;
        code_scope *new_scope = create_scope("DO-WHILE", NULL);
        push(new_scope);
        syntaxMKscope(tree->left);
        syntaxMKscope(tree->right);
        current_scope--;
    }
    
    else if (!strcmp(tree->token,"CODE BLOCK")) {
        current_scope++;
        code_scope *new_scope = create_scope("CODE-BLOCK", NULL);
        push(new_scope);
        syntaxMKscope(tree->left);
        syntaxMKscope(tree->right);
        current_scope--;
    }
    
    else if (!strcmp(tree->token, "IF")) {
        addCode(tree, NULL, NULL, freshLabel(), NULL, NULL);
        boolean_statement_check(tree->left); // IF VARS //
        syntaxMKscope(tree->right);
    }
    
    else if (!strcmp(tree->token, "IF-BODY")) {
        current_scope++;
        code_scope *scope = stack;
        code_scope *new_scope = create_scope("IF", NULL);
        push(new_scope);
        syntaxMKscope(tree->left);
        syntaxMKscope(tree->right);
        current_scope--;
    }
    
    else if (!strcmp(tree->token, "ELSE")) {
        syntaxMKscope(tree->left); // MOVE TO ELSE BODY //
    }
    
    else if (!strcmp(tree->token, "ELSE-BODY")) {
        current_scope++;
        code_scope *scope = stack;
        code_scope *new_scope = create_scope("ELSE", NULL);
        push(new_scope);
        syntaxMKscope(tree->left);
        syntaxMKscope(tree->right);
        current_scope--;
    }
    
    else if (!strcmp(tree->token, "RETURN")) {
        current_scope++;
        code_scope* code = stack;
        while (strcmp(code->name,"FUNCTION")) {
            code = code->previous_scope;
        }
        return_check(tree->left, code->func->return_type);
        current_scope--;
    }
    
    else if (!strcmp(tree->token, "|")) {
    }
    
    else if (!strcmp(tree->token, "ASSIGNMENT")) {
        assignment(tree->left);
    }
    
    else if (!strcmp(tree->token, "FOR")) {
        for_initialization_check(tree->left->left);
        for_condition_check(tree->left->right->left);
        for_initialization_check(tree->left->right->right);
        current_scope++;
        code_scope *scope = stack;
        code_scope *new_scope = create_scope("FOR", NULL);
        push(new_scope);
        syntaxMKscope(tree->right->left); // MOVING INTO FOR BODY //
        syntaxMKscope(tree->right->right); // MOVING INTO FOR BODY //
        current_scope--;
    }
    else {
        syntaxMKscope(tree->left);
        syntaxMKscope(tree->right);
    }
}

void add_varaiable(node* tree) {
    if (!strcmp(tree->token, "STRING")) {
        add_string_varaiable(tree->left);
    }
    else {
        char *var_type = tree->token;
        var_type[strlen(tree->token)] = '\0'; // VAR TYPE //
        add_varaiable_rec(tree->left, var_type);
    }
}

void add_string_varaiable(node* tree) {
    while (tree) {
        if (var_redeclaration_check(tree->token)) {
            stack->var->name[stack->countvar] = strdup(tree->token);
            stack->var->type[stack->countvar] = "STRING";
            char* type = var_declaration_check(tree->right->token, current_scope);
            if (strcmp(type, "INT")) {
                printf("\033[31mError - string index expects type 'INT', but index type '%s'.\n\033[0m", type);
                exit(1);
                //stack->var->len[stack->countvar++] = NULL;
            }
            else stack->var->len[stack->countvar++] = strdup(tree->right->token);
        }
        tree = tree->left;
    }
}

void assignment(node* tree) {
    char* type;
    if (!strcmp(tree->left->token, "PTR") || !strcmp(tree->left->token, "&")) {
        type = var_declaration_check(tree->left->left->token, current_scope);
    } else {
        type = var_declaration_check(tree->left->token, current_scope);
    }
    if (strcmp(type, "NULL")) {
        if (!strcmp(type, "STRING")) {
            if (tree->left->left != NULL) {
                char *index_type = var_declaration_check(tree->left->left->token, current_scope);
                if (strcmp(index_type, "INT")) {
                    printf("\033[31mError - string index expects type 'INT', but index type '%s'.\n\033[0m", type);
                    exit(1);
                }
                else insert_values_into_vars(tree->right, "CHAR");
            }
            else insert_values_into_vars(tree->right, "STRING");
        }
        else {
            insert_values_into_vars(tree->right, type);
        }
    }
}

void add_varaiable_rec(node* tree, char* var_type) {
    if (tree == NULL) { /*FINISH*/ }
    else if (!strcmp(tree->token, "=")) {
        if (var_redeclaration_check(tree->left->token)) {
            stack->var->name[stack->countvar] = (char*)malloc(sizeof(char));
            stack->var->name[stack->countvar] = tree->left->token;
            stack->var->name[stack->countvar][strlen(tree->left->token)] = '\0';
            stack->var->type[stack->countvar] = var_type;
            stack->var->value[stack->countvar] = (char*)malloc(sizeof(char));
            stack->var->value[stack->countvar] = insert_values_into_vars(tree->right, var_type);
            stack->var->len[stack->countvar++] = NULL;
        }
    }
    else if (!strcmp(tree->token, "ASSIGN-DECLER")) {
        add_varaiable_rec(tree->left, var_type);
        add_varaiable_rec(tree->right, var_type);
    }
    else { // ID TOKEN DOESN'T HAVE A RIGHT NODE //
        if (var_redeclaration_check(tree->token)) {
            stack->var->name[stack->countvar] = (char*)malloc(sizeof(char));
            stack->var->name[stack->countvar] = tree->token;
            stack->var->name[stack->countvar][strlen(tree->token)] = '\0';
            stack->var->value[stack->countvar] = NULL;
            stack->var->type[stack->countvar] = var_type;
            stack->var->len[stack->countvar++] = NULL;
        }
        add_varaiable_rec(tree->left, var_type);
    }
}

// CHECKS IF THE FUNCTION ALREADY EXIST IN THE CURRENT SCOPE //
void func_existence_check(char* token) {
    int scope_level = current_scope;
    code_scope *search = stack;
    while (search->scope_level != 0) {
        if (search->scope_level == scope_level && !strcmp(search->func->name, token)) {
            printf("\033[31mFunction %s() already exists.\n\033[0m", token);
            exit(1);
        }
        search = search->previous_scope;
    }
}

char* func_call_existence_check(char* token, int scope_level, node* tree) {
    code_scope *search;
    while (scope_level != 0) {
        search = stack;
        while(search->scope_level != 1) {
            if (search->func) {
                if (search->scope_level == scope_level && !strcmp(search->func->name, token)) {
                    func_call_args_check(search, tree);
                    return search->func->return_type;
                }
            }
            search = search->previous_scope;
        }
        scope_level--;
    }
    search = stack;
    while (search->scope_level != 0) {
        if (search->scope_level == 1 && !strcmp(search->func->name, token)) {
            func_call_args_check(search, tree);
            return search->func->return_type;
        }
        search = search->previous_scope;
    }
    printf("\033[31mFunction Error - function %s() doesn't exist.\n\033[0m", token);
    exit(1);
    return NULL;
}

void func_call_args_check(code_scope *scope, node* tree) {
    int func_call_args_count = 0;
    if (tree->right) {
        node* check = tree;
        char *type = var_declaration_check(check->right->token, current_scope);
        func_call_args_type_check(scope, func_call_args_count, type, check->right->token);
        check = check->right->left;
        func_call_args_count++;
        while (check) {
            char *type2 = var_declaration_check(check->token, current_scope);
            func_call_args_type_check(scope, func_call_args_count, type2, check->token);
            check = check->left;
            func_call_args_count++;
        }
    }
    if (func_call_args_count != scope->func->count_args) {
        printf("\033[31mFunction Error - amount of args in func call %s() don't match.\n\033[0m", scope->func->name);
        exit(1);
    }
}

void func_call_args_type_check(code_scope *scope, int index, char* type , char* token) {
    if (index >= scope->func->count_args) { return; }
    if (strcmp(scope->func->args->type[index], type)) {
        printf("\033[31mFunction Error - function ‘%s()’ expects type '%s', but argument %d has type '%s'.\n\033[0m", scope->func->name,scope->func->args->type[index],index+1, type);
        exit(1);
    }
}

char* insert_values_into_vars(node* tree, char* var_type) {
    char *value = (char*)malloc(sizeof(char));
    if (!strcmp(tree->token, "+") || !strcmp(tree->token, "-") || !strcmp(tree->token, "*") || !strcmp(tree->token, "/")) {
        if (!strcmp(var_type, "CHAR") || !strcmp(var_type, "STRING")) {
            printf("\033[31mError - can't use operator '%s' on '%s' type variables.\n\033[0m", tree->token, var_type);
        }
        strcat(value, insert_values_into_vars(tree->left, var_type));
        strcat(value,tree->token);
        strcat(value,insert_values_into_vars(tree->right, var_type));
        return value;
    }
    else if (!strcmp(tree->token, "()")) {
        return insert_values_into_vars(tree->right, var_type);
    }
    
    else if (!strcmp(tree->token, "NULL")) {
        if (strcmp(var_type, "INTP") && strcmp(var_type, "CHARP") && strcmp(var_type, "REALP")) {
            printf("\033[31mError - can't assign 'NULL' to %s.\n\033[0m", var_type);
            exit(1);
        }
    }
    else if (!strcmp(tree->token, "PTR")) {
        char *type = strdup(var_declaration_check(tree->left->token, current_scope));
        if (strcmp(type, "INTP") && strcmp(type, "CHARP") && strcmp(type, "REALP")) {
            printf("\033[31mError - can't add '*' to %s.\n\033[0m", tree->left->token);
            exit(1);
        }
        else if (strcmp(type, var_type)) {
            printf("\033[31mError - can't assign %s%s to '%s' type.\n\033[0m", tree->token,tree->left->token, var_type);
            exit(1);
        }
        char *value = (char*)malloc(sizeof(char));
        value = strcpy(value, "*"); strcat(value, tree->left->token);
        return value;
    }
    else if (!strcmp(tree->token, "FUNC CALL")) {
        current_scope++;
        char *type = func_call_existence_check(tree->left->token, current_scope, tree);
        if (strcmp(type, var_type)) {
            printf("\033[31mError - function %s() can't return '%s' to '%s'.\n\033[0m", tree->left->token, type, var_type);
            exit(1);
        }
        current_scope--;
        return tree->left->token;
    }
    else if (!strcmp(tree->token, "|")) {
        char *type = var_declaration_check(tree->left->token, current_scope);
        if (strcmp(type, "STRING") || strcmp(var_type, "INT")) {
            printf("\033[31mError - can't assign length of %s %s to '%s' type.\n\033[0m", type, tree->left->token, var_type);
            exit(1);
        }
        return tree->left->token;
    }
    else if (!strcmp(tree->token, "&")) {
        char *type = var_declaration_check(tree->right->token, current_scope);
        if (!strcmp(type, "BOOL") || !strcmp(type, "INTP") || !strcmp(type, "CHARP") ||!strcmp(type, "REALP")) {
            printf("\033[31mError - can't use '&' on '%s' type.\n\033[0m", type);
            exit(1);
        }
    }
    else if (!strcmp(tree->token, "!")) {
            char *type = var_declaration_check(tree->right->token, current_scope);
            if (strcmp(type, "BOOL")) {
                printf("\033[31mError - can't use '!' on '%s' type.\n\033[0m", type);
                exit(1);
            }
    }
    
    else if (!strcmp(tree->token, "<") || !strcmp(tree->token, ">") || !strcmp(tree->token, "<=") || !strcmp(tree->token, ">=") || !strcmp(tree->token, "<==") || !strcmp(tree->token, "!=")) {
        printf("\033[31mError - Can't use %s in assignment.\n\033[0m", tree->token);
        exit(1);
    }
    else {
        char *type = strdup(var_declaration_check(tree->token, current_scope));
        if (strcmp(type, var_type)) {
            printf("\033[31mError - can't assign %s to '%s' type.\n\033[0m", tree->token, var_type);
            exit(1);
        }
        return tree->token;
    }
}

void return_check(node* tree, char* var_type) {
    char *value = (char*)malloc(sizeof(char));
    if (!strcmp(tree->token, "+") || !strcmp(tree->token, "-") || !strcmp(tree->token, "*") || !strcmp(tree->token, "/")) {
        return_check(tree->left, var_type);
        return_check(tree->right, var_type);
    }
    else if (!strcmp(tree->token, "FUNC CALL")) {
        current_scope++;
        char *type = func_call_existence_check(tree->left->token, current_scope, tree);
        if (strcmp(type, var_type)) {
            printf("\033[31mError - function %s() can't return '%s' to '%s'.\n\033[0m", tree->left->token, type, var_type);
            exit(1);
        }
        current_scope--;
    }
    else {
        char *type = strdup(var_declaration_check(tree->token, current_scope));
        if (strcmp(type, var_type)) {
            printf("\033[31mFunction Error - %s %s is not a valid return type.\n\033[0m", type, tree->token);
            exit(1);
        }
    }
}

// CHECKS IF THE VALUE HAS ALREADY BEEN DECLARED IN THE SCOPE //
bool var_redeclaration_check(char* token) {
    code_scope *search = stack;
    int level = current_scope;
    while (strcmp(search->name, "FUNCTION") != 0 && strcmp(search->name, "CODE-BLOCK") != 0) {
        if (search->scope_level == level) {
            for (int i = 0; i < search->countvar; i++) {
                if (!strcmp(token, search->var->name[i])) {
                    printf("\033[31mError - redeclaration of %s.\n\033[0m", token);
                    exit(1);
                    return false;
                    
                }
            }
        }
        search = search->previous_scope;
        level--;
    }
    for (int i = 0; i < search->countvar; i++) {
        if (!strcmp(token, search->var->name[i])) {
            printf("\033[31mError - redeclaration of %s.\n\033[0m", token);
            exit(1);
            return false;
        }
    }
    return true;
}

// CHECKS IF THE VALUE HAS BEEN DECLARED IN THE HIS SCOPE OR IN THE PREVIOUS SCOPES //
char* var_declaration_check(char* token, int scope_level) {
    int n;
    if (token[0] == '\"') { return "STRING"; }
    char* dot_position = strchr(token, '.');
    if (dot_position) { return "REAL"; }
    if (sscanf(token, "%d", &n) == 1) { return "INT"; } // IF THE TOKEN IS A NUMBER - NO DECLERATION CHECK IS NEEDED //
    if (token[0] == '\'' && token[2] == '\'') { return "CHAR"; }
    if (!strcmp(token, "TRUE") || !strcmp(token, "true") || !strcmp(token, "FALSE") || !strcmp(token, "false") ) { return "BOOL"; }
    if (scope_level == 0) {
        printf("\033[31mError - '%s' is not declered.\n\033[0m", token);
        exit(1);
        return "NULL";
    }
    code_scope *search = stack;
    while (search->scope_level != 0) {
        if (search->scope_level == scope_level) {
            break;
        }
        search = search->previous_scope;
    }
    for (int i = 0; i < search->countvar; i++) {
        if (!strcmp(token, search->var->name[i])) {
            return search->var->type[i];
        }
    }
    return var_declaration_check(token, scope_level-1);
}

code_scope* create_scope(char* name, Function* function) {
    code_scope *scope = (code_scope*)malloc(sizeof(code_scope));
    scope->name = name;
    scope->scope_level = current_scope;
    scope->var = (Varaiable*)malloc(sizeof(Varaiable));
    scope->var->name = (char**)malloc(sizeof(char*) * 256);
    scope->var->value = (char**)malloc(sizeof(char*) * 256);
    scope->var->type = (char**)malloc(sizeof(char*) * 256);
    scope->var->len = (char**)malloc(sizeof(char*) * 256);
    scope->func = function;
    scope->countvar = 0;
    if (function != NULL) {
        for (int i = 0; i < scope->func->count_args; i++) {
            scope->var->name[scope->countvar] = strdup(scope->func->args->name[i]);
            scope->var->type[scope->countvar] = strdup(scope->func->args->type[i]);
            scope->countvar++;
        }
    }
    scope->next_scope = NULL;
    scope->previous_scope = NULL;
    return scope;
}

Function* create_function(char *name, Arguments *args, char *return_type, int count_args, code_scope *code_scope) {
    Function *function = (Function*)malloc(sizeof(Function));
    function->name = name;
    function->args = args;
    function->return_type = strdup(return_type);
    function->count_args = count_args;
    return function;
}

void push(code_scope *new_scope) {
    if (new_scope == NULL) {
        stack = mkcode("CODE");
    }
    else {
        new_scope->previous_scope = stack;
        stack = NULL;
        stack = new_scope;
    }
}

Arguments* mkargs(node *tree, int *args_count) {
    Arguments *args = (Arguments*)malloc(sizeof(Arguments));
    args->type = (char**)malloc(sizeof(char*) * 256);
    args->name = (char**)malloc(sizeof(char*) * 256);
    args->len = (char**)malloc(sizeof(char*) * 256);
    char *token = NULL ,*type_token = NULL;
    int index = 0;
    while (tree) {
        type_token = strdup(tree->token);
        node* right = tree->right;
        while (right) {
            args->type[index] = strdup(type_token);
            args->name[index] = malloc(sizeof(right->token) + 1);
            args->name[index++] = strdup(right->token);
            right = right->right;
            (*args_count)++;
        }
        tree = tree->left;
    }
    return args;
}

        
code_scope* mkcode(char* name) {
    code_scope *newlevel = (code_scope*)malloc(sizeof(code_scope));
    newlevel->scope_level = current_scope;
    newlevel->name = name;
    newlevel->var = NULL;
    newlevel->countvar = 0;
    newlevel->func = NULL;
    newlevel->countfunc = 0;
    newlevel->next_scope = NULL;
    newlevel->previous_scope = NULL;
    return newlevel;
}

int yyerror(char *e) {
	int yydebug = 1;
	fflush(stdout);
	fprintf(stderr,"Error %s at line %d\n" ,e, yylineno);
	fprintf(stderr, "Does not accept '%s'\n",(yytext));
	return 0;
}

void boolean_statement_check(node* tree) {
    char *left, *right;
    if (tree != NULL){
        if ((!strcmp(tree->token,"<") || !strcmp(tree->token,">") || !strcmp(tree->token,"==") || !strcmp(tree->token,"!=")
		|| !strcmp(tree->token,"<=") || !strcmp(tree->token,">=")))
        {
            if (!strcmp(tree->left->token,"<") || !strcmp(tree->left->token,">") || !strcmp(tree->left->token,"==") || !strcmp(tree->left->token,"!=") || !strcmp(tree->left->token,">=") || !strcmp(tree->left->token,"<=")) {
                printf("Error. Two boolean operators following each other. '%s' followed by '%s'.\n", tree->left->token, tree->token);
                exit(1);
            }
            else if (!strcmp(tree->left->token,"()")){
                boolean_statement_check(tree->left->right);
            }
            else if (!strcmp(tree->left->token,"+") || !strcmp(tree->left->token,"-") || !strcmp(tree->left->token,"*")
            || !strcmp(tree->left->token,"/")){
                left = arithmetic_statement_check(tree->left);
            }
			else if (!strcmp(tree->left->token,"|")){
				left = "INT";
			}
            else {
                left = var_declaration_check(tree->left->token,current_scope);
            }
            if (!strcmp(tree->right->token,"<") || !strcmp(tree->right->token,">") || !strcmp(tree->right->token,"==") || !strcmp(tree->right->token,"!=") || !strcmp(tree->right->token,">=") || !strcmp(tree->right->token,"<=")) {
                printf("Error. Two boolean operators following each other. '%s' followed by '%s'.\n", tree->left->token, tree->token);
                exit(1);
            }
            else if (!strcmp(tree->right->token,"()") ){
                boolean_statement_check(tree->right->right);
            }
            else if (!strcmp(tree->right->token,"+") || !strcmp(tree->right->token,"-") || !strcmp(tree->right->token,"*")
            || !strcmp(tree->right->token,"/")){
                right = arithmetic_statement_check(tree->right);
            }
			else if (!strcmp(tree->right->token,"|")){
				right = "INT";
			}
            else {
                right = var_declaration_check(tree->right->token,current_scope);
            }
            if ((!strcmp(left,"REAL") && !strcmp(right,"INT")) || (!strcmp(right,"REAL") && !strcmp(left,"INT"))){
                printf("Warning. Comparison of '%s' and '%s' may cause precision loss.\n", left, right);
            }
            else if (!strcmp(tree->token,"==") || !strcmp(tree->token, "!=")){
                if (strcmp(left,right)) {
                    printf("Error. Cannot us boolean comparison on '%s' and '%s' type.\n",left,right);
                    exit(1);
                }
            }
            else if (!strcmp(tree->token,"<") || !strcmp(tree->token, ">") || !strcmp(tree->token, ">=") || !strcmp(tree->token, "<=")){
                if (strcmp(left,"INT") && strcmp(left,"REAL")){
                    printf("Error. Cannot use boolean comparison on '%s' type.\n", left);
                    exit(1);
                }
                if (strcmp(right,"INT") && strcmp(right,"REAL")){
                    printf("Error. Cannot use boolean comparison on '%s' type.\n", right);
                    exit(1);
                }
            }
            
        }
        else if (!strcmp(tree->token,"||") || !strcmp(tree->token,"&&")){
            boolean_statement_check(tree->left);
            boolean_statement_check(tree->right);
            return;
        }
        else if (!strcmp(tree->token,"()")){
            if (tree->left == NULL && tree->right == NULL) {
                printf("\033[31mEmpty '()' unacceptable in boolean statement.\033[0m\n");
                exit(1);
            }
            if (tree->left != NULL) { boolean_statement_check(tree->left); };
            if (tree->right != NULL) { boolean_statement_check(tree->right); };
        }
        else if (strcmp(var_declaration_check(tree->token,current_scope),"BOOL")){ /*boolean*/
            printf("\033[31mError. Parameter %s has to be boolean.\033[0m\n", tree->token);
            exit(1);
        }
        
    }
}

char* arithmetic_statement_check(node* tree) {
    char *left = NULL, *right = NULL;
    //TOKEN??
    if (!strcmp(tree->token,"()")){
        if (tree->left == NULL && tree->right == NULL) {
                printf("\033[31mEmpty '()' unacceptable in arithmetic statement.\033[0m\n");
                exit(1);
              }
            if (tree->left != NULL) { return arithmetic_statement_check(tree->left); };
            if (tree->right != NULL) { return arithmetic_statement_check(tree->right); };
    }
    else if (!strcmp(tree->token,"&&") || !strcmp(tree->token,"||") || !strcmp(tree->token,"<") || !strcmp(tree->token,">")
		 || !strcmp(tree->token,"<=") || !strcmp(tree->token,">=")){
        printf("Boolean symbol %s is not allowed in arithmetic statement.\n", tree->token);
        exit(1);
    }
    else if (!strcmp(tree->token,"+") || !strcmp(tree->token, "-") || !strcmp(tree->token, "*") || !strcmp(tree->token, "/")){
        //LEFT//
        if (!strcmp(tree->left->token,"+") || !strcmp(tree->left->token,"-") || !strcmp(tree->left->token,"*")
            || !strcmp(tree->left->token,"/") || !strcmp(tree->left->token,"&&") || !strcmp(tree->left->token,"||")
            || !strcmp(tree->left->token,"<") || !strcmp(tree->left->token,">") || !strcmp(tree->left->token,"<=") 
		|| !strcmp(tree->left->token,">=") || !strcmp(tree->left->token, "()")){
            left = arithmetic_statement_check(tree->left);
        }
		else if (!strcmp(tree->left->token,"|")){
				left = "INT";
		}
        else {
            left = var_declaration_check(tree->left->token,current_scope);
        }
        //RIGHT//
        if (!strcmp(tree->right->token,"+") || !strcmp(tree->right->token,"-") || !strcmp(tree->right->token,"*")
            || !strcmp(tree->right->token,"/") || !strcmp(tree->right->token,"&&") || !strcmp(tree->right->token,"||")
            || !strcmp(tree->right->token,"<") || !strcmp(tree->right->token,">") || !strcmp(tree->right->token,"<=") || 
		!strcmp(tree->right->token,">=") || !strcmp(tree->right->token, "()")){
            right = arithmetic_statement_check(tree->right);
        }
		else if (!strcmp(tree->right->token,"|")){
				right = "INT";
		}
        else {
            right = var_declaration_check(tree->right->token,current_scope);
        }
        if (!strcmp(left,"INT") && !strcmp(right,"INT")) {
            return "INT";
        }
        else if ((!strcmp(left,"REAL") && !strcmp(right,"REAL")) || (!strcmp(left,"REAL") && !strcmp(right,"INT"))
            || (!strcmp(left,"INT") && !strcmp(right,"REAL"))){
            return "REAL";
        }
        else {
            if (strcmp(left,"REAL") && strcmp(left,"INT")) {
                printf("Error. Cannot use arithmetic symbol '%s' on '%s' types.\n", tree->token, left);
                exit(1);
            }
            else if (strcmp(right,"REAL") && strcmp(right,"INT")) {
                printf("Error. Cannot use arithmetic symbol '%s' on '%s' types.\n", tree->token, right);
                exit(1);
            }
            if (strcmp(left,"REAL") && strcmp(left,"INT")){
                return left;
            }
            else return right;
        }
    }
    else {
            //printf("var_dec: %s\n",var_declaration_check(tree->token,current_scope));
            return var_declaration_check(tree->token,current_scope);
    }
}


void for_initialization_check(node* tree) {
    if (tree == NULL) { /*FINISH*/ }
    else if (!strcmp(tree->token, "=")) {
        char *type = var_declaration_check(tree->left->token, current_scope);
        if (strcmp(type, "INT") && strcmp(type, "NULL") && strcmp(type, "REAL")) {
            printf("\033[31mFor Initialization/increment/decrement Error - Parameter %s has to be INT type or REAL type.\033[0m\n", tree->left->token);
            exit(1);
        }
        else {
            code_scope* scope = stack;
            while (scope->scope_level != 0) {
                if (scope->scope_level == current_scope) {
                    break;
                }
                scope = scope->previous_scope;
            }
            for (int i = 0; i < scope->countvar ; i++) {
                if (!strcmp(scope->var->name[i], tree->left->token)) {
                    scope->var->value[i] = strdup(insert_values_into_vars(tree->right, type));
                }
            }
        }
    }
}

void for_condition_check(node* tree) {
    boolean_statement_check(tree);
}

/* Part 3 */
void addCode(node* node, char *code, char *var, char *label, char *truelabel, char *falselabel) {
    if(code != NULL) {
        node->code = (char*)malloc(sizeof(char)*(1+strlen(code)));
        strcpy(node->code, code);
    }
    else if(node->code == NULL) {
        node->code = (char*)malloc(sizeof(char)*2);
        strcpy(node->code,"");
    }
    if(var != NULL) {
        node->var=(char*)malloc(sizeof(char)*(1+strlen(var)));
        strcpy(node->var, var);
    }
    else if(node->var == NULL) {
        node->var = (char*)malloc(sizeof(char)*2);
        strcpy(node->var, "");
    }
    if(label != NULL && node->label == NULL) {
        node->label = (char*)malloc(sizeof(char)*(1+strlen(label)));
        strcpy(node->label, label);
    }
    if(truelabel != NULL && node->truelabel == NULL) {
        node->truelabel = (char*)malloc(sizeof(char)*(1+strlen(truelabel)));
        strcpy(node->truelabel, truelabel);
    }
    if(falselabel != NULL && node->falselabel == NULL) {
        node->falselabel = (char*)malloc(sizeof(char)*(1+strlen(falselabel)));
        strcpy(node->falselabel, falselabel);
    }
}

void addCode2(node* node,char *code,char *var,char *label,char *truelabel,char *falselabel) {
    if(code != NULL){
        node->code = (char*)malloc(sizeof(char)*(1+strlen(code)));
        strcpy(node->code, code);
    }
    else if(node->code == NULL){
		node->code = (char*)malloc(sizeof(char)*2);
		strcpy(node->code, "");
    }

    if(var!=NULL){
		node->var=(char*)malloc(sizeof(char)*(1+strlen(var)));
		strcpy(node->var, var);
    }
    else if(node->var == NULL){
		node->var = (char*)malloc(sizeof(char)*2);
		strcpy(node->var, "");
    }

    if(label != NULL){
		node->label=(char*)malloc(sizeof(char)*(1+strlen(label)));
		strcpy(node->label,label);
    }

    if(truelabel != NULL){
		node->truelabel = (char*)malloc(sizeof(char)*(1+strlen(truelabel)));
		strcpy(node->truelabel, truelabel);
    }
		
    if(falselabel != NULL && node->falselabel == NULL){
		node->falselabel = (char*)malloc(sizeof(char)*(1+strlen(falselabel)));
		strcpy(node->falselabel, falselabel);
    }
}
	
char* freshVar(){
    char* x;
    asprintf(&x,"t%d", t++);
    return x;
}

char* freshLabel(){
    char* x;
    asprintf(&x,"L%d", l++);
    return x;
}

char* gen(char*s1, char*s2, char*s3, char*s4, char*s5) {
    char* x;
    asprintf(&x,"%s %s %s %s %s\n", s1, s2, s3, s4,s5);
    return x;
}
	
char* mystrcat(char*des,char*src){
    char* tamp = des;
    char* num;
    asprintf(&num,"%d ",line++);
    if(src!=NULL) {
        if(des==NULL){
            des=(char*)malloc((strlen(src)+1)*sizeof(char));
            strcpy(des,src);
            return des;
        }
		des=(char*)malloc((strlen(des)+strlen(src)+1+strlen(num))*sizeof(char));
		if(tamp!=NULL) {
            strcat(des,tamp);
		}
		if(src!=NULL) {
            strcat(des,src);
		}
    }
		return des;
}

char *replaceWord(const char *s, const char *oldW, const char *newW) {
    char *result; 
    int i, cnt = 0; 
    int newWlen = strlen(newW); 
    int oldWlen = strlen(oldW); 
  
    // Counting the number of times old word 
    // occur in the string 
    for (i = 0; s[i] != '\0'; i++) 
    { 
        if (strstr(&s[i], oldW) == &s[i]) 
        { 
            cnt++; 
  
            // Jumping to index after the old word. 
            i += oldWlen - 1; 
        } 
    } 
  
    // Making new string of enough length 
    result = (char *)malloc(i + cnt * (newWlen - oldWlen) + 1); 
  
    i = 0; 
    while (*s) 
    { 
        // compare the substring with the result
        if (strstr(s, oldW) == s) 
        { 
            strcpy(&result[i], newW); 
            i += newWlen; 
            s += oldWlen; 
        } 
        else
            result[i++] = *s++; 
    } 
  
    result[i] = '\0'; 
    return result; 
} 

 
char * mkspace(char *label) {
	char * msg;
	char x=' ';
	int len = strlen(mystrcat(label,"\0"));
	if(label==NULL || !strcmp(label,""))
		msg="";
	else
		{
		asprintf(&msg,"%c",x);
		while(len<5){
		asprintf(&msg,"%c%s",x,msg);
			len++;
		}
		asprintf(&label,"%s: \n",mystrcat(label,"\0"));
		msg=mystrcat(msg,label);
		}
		return msg;
}

int POPParams(Arguments *args, int args_amount){
    int size = 0;
    for(int i = 0 ; i < args_amount ; i++) {
        if(!strcmp(args->type[i],"INT"))
            size += 4;
        else if(!strcmp(args->type[i],"CHAR"))
            size += 1;
        else if(!strcmp(args->type[i],"REAL"))
            size += 8;
        else if(strcmp(args->type[i],"STRING")==0)
        size += atoi(args->len[i]);
        else if(strcmp(args->type[i],"BOOL")==0)
            size += 4;
        else if(strcmp(args->type[i],"INTP")==0)
            size += 4;
        else if(strcmp(args->type[i],"CHARP")==0)
            size += 4;
        else if(strcmp(args->type[i],"REALP")==0)
            size += 4;
    }
    return size;
}

int findfunc(char *func_name) {
    code_scope *search = stack;
    int size;
    while (search->scope_level != 0) {
        if (search->func) {
            if (!strcmp(func_name, search->func->name)) {
                break;
            }
        }
        search = search->previous_scope;
    }
    return POPParams(search->func->args, search->func->count_args);
}


void calc3AC(node* tree) {
    if (tree == NULL) { /* FINISH */ return; }
    
    if(!strcmp(tree->token, "=")) {
        //calc3AC(tree->left);
        calc3AC(tree->right);
        addCode(tree, mystrcat(tree->right->code, mystrcat(tree->left->token, gen(" =", tree->right->var, "", "",""))), NULL, NULL, NULL, NULL);
        //addCode(tree, mystrcat(tree->right->code, gen(tree->left->var, "=", tree->right->var,"","")), NULL, NULL, NULL, NULL);
	}
    
	else if(!strcmp(tree->token, "FUNCTION") || !strcmp(tree->token, "PROCEDURE")) {
        t = 0;
        calc3AC(tree->left->right);
        if (tree->right) {
            calc3AC(tree->right);
        }
        char *x;
        asprintf(&x,"\x1B[32m %s:\n\x1B[0m",tree->left->token);
        if (tree->right) {
            addCode(tree, mystrcat(mystrcat(x, tree->left->right->code), tree->right->code), NULL, NULL, NULL, NULL);
        }
        else {
            addCode(tree, mystrcat(x, tree->left->right->code), NULL, NULL, NULL, NULL);
        }
		return;
	}
    
	else if(!strcmp(tree->token, "CODE")) {
	 	calc3AC(tree->right); // GO TO FUNCTION //
        if(tree->right) {
			addCode(tree, tree->right->code, NULL, NULL, NULL, NULL);
        }
		tree->code=replaceWord(tree->code, "\n\n", "\n") ;
		char x='a',*y,*z;
        while (x <= 'z') {
            asprintf(&y,"\n%c",x);
            asprintf(&z,"\n\t%c",x);
			tree->code=replaceWord(tree->code, y, z) ;
			x++;
		}
		x = 'A';
        while (x <= 'Z') {
			asprintf(&y,"\n%c",x);
			asprintf(&z,"\n\t%c",x);
			tree->code=replaceWord(tree->code, y, z) ;
			x++;
		}
        printf("%s\n", tree->code);
		return;
	}
    
    else if(!strcmp(tree->token, "BODY")) {
        if(tree->left != NULL) {
            calc3AC(tree->left);
        }
        if(tree->right != NULL) {
            calc3AC(tree->right);
        }
        if (tree->right && tree->left) {
            addCode(tree, mystrcat(mystrcat("\tBeginFunc\n", mystrcat(tree->left->code, tree->right->code)), "\tEndFunc\n"), tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
        else if (tree->left && !tree->right) {
            addCode(tree, mystrcat(mystrcat("\tBeginFunc\n", tree->left->code), "\tEndFunc\n"), tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
        else if (!tree->left && tree->right) {
            addCode(tree, mystrcat(mystrcat("\tBeginFunc\n", tree->right->code), "\tEndFunc\n"), tree->right->var, tree->right->label, tree->right->truelabel, tree->right->falselabel);
        }
        else {
            addCode(tree, mystrcat("\tBeginFunc\n", "\tEndFunc\n"), NULL, NULL, NULL, NULL);
        }
		return;
	}
    
    else if (!strcmp(tree->token, "FUNC CALL")) {
        if(tree->right!=NULL) calc3AC(tree->right);
        node* args_run = tree->right;
        while (args_run != NULL) {
            addCode(tree, mystrcat(tree->code, gen(args_run->var, "=",args_run->token,"","")), NULL, NULL, NULL, NULL);
            addCode(tree, mystrcat(tree->code, mystrcat("PushParam ",mystrcat(args_run->var,"\n"))),NULL,NULL,NULL,NULL);
            args_run = args_run->left;
        }
        tree->sum = findfunc(tree->left->token);
        char *x;
        addCode(tree, NULL, freshVar(), NULL, NULL, NULL);
        asprintf(&x,"%s = LCALL %s\n‪\tPopParams %d‬‬‬‬\n", tree->var, tree->left->token, tree->sum);
        addCode(tree,mystrcat(tree->code,x),NULL,NULL,NULL,NULL);
        return;
    }
    
    else if(!strcmp(tree->token, "STATEMENT")) {
        calc3AC(tree->left);
        calc3AC(tree->right);
        if (tree->right) {
            addCode(tree, mystrcat(tree->left->code,tree->right->code),tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
        else {
            addCode(tree, tree->left->code,tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
    }
    
    else if (!strcmp(tree->token, "ASSIGNMENT")) {
        calc3AC(tree->left);
        addCode(tree, tree->left->code, tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
    }
    
    else if (!strcmp(tree->token ,"DECLETARION")) {
        if (!strcmp(tree->left->token, "STRING")) {
            calc3AC(tree->left);
            addCode(tree, tree->left->code, tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
        else {
            node* search = tree;
            while (search) {
                if (!strcmp(search->token, "ASSIGN-DECLER")) {
                    calc3AC(search->left);
                    addCode(tree, mystrcat(search->code, search->left->code), search->left->var, search->left->label, search->left->truelabel, search->left->falselabel);
                }
                search = search->left;
            }
        }
    }
    
    else if (!strcmp(tree->token ,"STRING")) {
        if (tree->left->right->right) {
            calc3AC(tree->left->right->right);
            addCode(tree, NULL, freshVar(), NULL, NULL, NULL);
            addCode(tree, mystrcat(tree->left->right->right->code, gen(tree->left->token, "=", tree->left->right->right->var, "","")), NULL, NULL, NULL, NULL);
            return;
        }
    }
    
    else if (!strcmp(tree->token ,"ASSIGN-DECLER")) {
        calc3AC(tree->left);
        addCode(tree, tree->left->code, tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
    }
    
    else if(!strcmp(tree->token, "+") || !strcmp(tree->token, "-") || !strcmp(tree->token, "/") || !strcmp(tree->token, "*")) {
        if(!strcmp(tree->left->token, "+") || !strcmp(tree->left->token, "-") || !strcmp(tree->left->token, "/") || !strcmp(tree->left->token, "*")) {
            calc3AC(tree->left);
            calc3AC(tree->right);
            addCode(tree, NULL, freshVar(), NULL, NULL, NULL);
            addCode(tree, mystrcat(tree->left->code, tree->right->code), NULL, NULL, NULL, NULL);
            addCode(tree, mystrcat(tree->code, gen(tree->var, "=", tree->left->var, tree->left->token, tree->right->var)), NULL, NULL, NULL, NULL);
        }
        else {
            addCode(tree, NULL, freshVar(), NULL, NULL, NULL);
            addCode(tree, mystrcat(mystrcat(tree->left->code, tree->right->code), gen(tree->var, "=", tree->left->token, tree->token, tree->right->token)), NULL, NULL, NULL, NULL);
        }
        return;
    }
    
    else if(!strcmp(tree->token, ">") || !strcmp(tree->token, "<") || !strcmp(tree->token, "==") || !strcmp(tree->token, ">=") || !strcmp(tree->token, "<=") || !strcmp(tree->token, "!="))
    {
        if(!strcmp(tree->left->token, "+") || !strcmp(tree->left->token, "-") || !strcmp(tree->left->token, "/") || !strcmp(tree->left->token, "*")) {
            calc3AC(tree->left);
        }
        calc3AC(tree->right);

        addCode(tree, NULL, freshVar(), NULL, NULL, NULL);
        addCode(tree,  mystrcat(mystrcat(tree->left->code,tree->right->code), gen(tree->var, "=",tree->left->token,tree->token ,tree->right->var)),NULL,NULL,NULL,NULL);
        addCode(tree, mystrcat(tree->code,gen("ifz", tree->var, "goto", tree->falselabel,"\n")), tree->left->var, NULL, NULL, NULL);
    }
    
    else if (!strcmp(tree->token, "IF")) {
        
        calc3AC(tree->left);
        calc3AC(tree->right);
        if(tree->left) addCode(tree->left,NULL,NULL,NULL,NULL,tree->label);
        if(tree->right) addCode(tree->right,NULL,NULL,NULL,tree->label,NULL);
    addCode(tree,mystrcat(tree->left->code,mystrcat(mkspace(tree->left->label),mystrcat(mkspace(tree->left->truelabel),mystrcat(mkspace(tree->left->right->truelabel),mystrcat(mkspace(tree->truelabel),mystrcat(tree->right->code,mkspace(tree->truelabel))))))),NULL,NULL,NULL,NULL);

        addCode(tree, mystrcat(tree->code, mkspace(tree->left->falselabel)), NULL, NULL, NULL, NULL);
        return;
    }
    
    else if (!strcmp(tree->token, "IF-ELSE")) {
        calc3AC(tree->left->left);
        calc3AC(tree->left->right);
        calc3AC(tree->right);
        addCode(tree, tree->left->left->code, NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, mystrcat(mkspace(tree->left->left->right->truelabel),tree->left->right->code)), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, gen("goto", tree->left->left->truelabel, "","","")), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, mkspace(tree->left->left->falselabel)), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, tree->right->code), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, mkspace(tree->left->left->truelabel)), NULL, NULL, NULL, NULL);
    }
    
    else if (!strcmp(tree->token, "ELSE")) {
        calc3AC(tree->left);
        addCode(tree, tree->left->code, tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
    }
    
    else if (!strcmp(tree->token, "WHILE")) {
        if(tree->left) calc3AC(tree->left);
        if(tree->right) calc3AC(tree->right);
        addCode(tree, mystrcat(mkspace(tree->left->truelabel), tree->left->code), NULL, NULL, NULL, freshLabel());
        addCode(tree, mystrcat(tree->code, mystrcat(mkspace(tree->left->right->truelabel),tree->right->code)), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, mystrcat("goto ", tree->left->truelabel)),  NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, "\n"), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, mkspace(tree->left->falselabel)), NULL, NULL, NULL, NULL);
        return;
    }
    
    else if (!strcmp(tree->token, "WHILE-BODY")) {
        if (tree->left) calc3AC(tree->left);
        if (tree->right) calc3AC(tree->right);
        if (tree->left && tree->right) {
            addCode(tree, mystrcat(tree->left->code, tree->right->code), tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
        else if (!tree->left && tree->right) {
            addCode(tree, tree->right->code, tree->right->var, tree->right->label, tree->right->truelabel, tree->right->falselabel);
        }
        else if (tree->left && !tree->right) {
            addCode(tree, tree->left->code, tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
        else {
            addCode(tree, "", tree->token, NULL, NULL, NULL);
        }
    }
    
    else if (!strcmp(tree->token, "DO-WHILE")) {
        calc3AC(tree->left);
        calc3AC(tree->right);
        addCode(tree, mystrcat(mkspace(tree->right->truelabel), tree->left->code), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, tree->right->code), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, mystrcat("goto ",tree->right->truelabel)), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, "\n"), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, mkspace(tree->right->falselabel)), NULL, NULL, NULL, NULL);
    }
    
    else if (!strcmp(tree->token, "DO-WHILE-BODY")) {
        if (tree->left) calc3AC(tree->left);
        if (tree->right) calc3AC(tree->right);
        if (tree->left && tree->right) {
            addCode(tree, mystrcat(tree->left->code, tree->right->code), tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
        else if (!tree->left && tree->right) {
            addCode(tree, tree->right->code, tree->right->var, tree->right->label, tree->right->truelabel, tree->right->falselabel);
        }
        else if (tree->left && !tree->right) {
            addCode(tree, tree->left->code, tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
        else {
            addCode(tree, "", tree->token, NULL, NULL, NULL);
        }
    }
    
    else if (!strcmp(tree->token, "FOR")) {
        calc3AC(tree->left->left); // GO TO FOR-INIT //
        calc3AC(tree->left->right->left); // GO TO FOR-CONDITION //
        calc3AC(tree->right); // GO TO BODY //
        calc3AC(tree->left->right->right); // GO TO ADVENCMENT //
        
        addCode(tree, mystrcat(tree->code, tree->left->left->code), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, mystrcat(mkspace(tree->left->right->left->truelabel), tree->left->right->left->code)), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, tree->right->code), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, tree->left->right->right->code), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, mystrcat("goto ",tree->left->right->left->truelabel)), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, "\n"), NULL, NULL, NULL, NULL);
        addCode(tree, mystrcat(tree->code, mkspace(tree->left->right->left->falselabel)), NULL, NULL, NULL, NULL);
    }
    
    else if (!strcmp(tree->token, "FOR-BODY")) {
        if (tree->left) calc3AC(tree->left);
        if (tree->right) calc3AC(tree->right);
        if (tree->left && tree->right) {
            addCode(tree, mystrcat(tree->left->code, tree->right->code), tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
        else if (!tree->left && tree->right) {
            addCode(tree, tree->right->code, tree->right->var, tree->right->label, tree->right->truelabel, tree->right->falselabel);
        }
        else if (tree->left && !tree->right) {
            addCode(tree, tree->left->code, tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
        else {
            addCode(tree, "", tree->token, NULL, NULL, NULL);
        }
    }
    
    
    else if (!strcmp(tree->token, "ELSE-BODY")) {
        calc3AC(tree->left);
        addCode(tree, tree->left->code, tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
    }
    
    else if (!strcmp(tree->token, "IF-BODY")) {
        if (tree->left) calc3AC(tree->left);
        if (tree->right) calc3AC(tree->right);
        if (tree->left && tree->right) {
            addCode(tree, mystrcat(tree->left->code, tree->right->code), tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
        else if (!tree->left && tree->right) {
            addCode(tree, tree->right->code, tree->right->var, tree->right->label, tree->right->truelabel, tree->right->falselabel);
        }
        else if (tree->left && !tree->right) {
            addCode(tree, tree->left->code, tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
        else {
            addCode(tree, "", tree->token, NULL, NULL, NULL);
        }
    }
    else if (!strcmp(tree->token, "RETURN")) {
        if(!strcmp(tree->token, "+") || !strcmp(tree->token, "-") || !strcmp(tree->token, "/") || !strcmp(tree->token, "*")) {
            calc3AC(tree->left);
            addCode(tree, mystrcat(tree->left->code, gen("RETURN", tree->left->var, "","","")), NULL, NULL, NULL, NULL);
        }
        else {
            addCode(tree, mystrcat(tree->left->code, gen("RETURN", tree->left->token, "","","")), NULL, NULL, NULL, NULL);
        }
    }
    
    else if (!strcmp(tree->token, "CODE BLOCK")) {
        if(tree->left != NULL) {
            calc3AC(tree->left);
        }
        if(tree->right != NULL) {
            calc3AC(tree->right);
        }
        if (tree->right && tree->left) {
            addCode(tree, mystrcat(mystrcat("\tBeginCodeBlock\n", mystrcat(tree->left->code, tree->right->code)), "\tEndCodeBlock\n"), tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
        else if (tree->left && !tree->right) {
            addCode(tree, mystrcat(mystrcat("\tBeginCodeBlock\n", tree->left->code), "\tEndCodeBlock\n"), tree->left->var, tree->left->label, tree->left->truelabel, tree->left->falselabel);
        }
        else if (!tree->left && tree->right) {
            addCode(tree, mystrcat(mystrcat("\tBeginCodeBlock\n", tree->right->code), "\tEndCodeBlock\n"), tree->right->var, tree->right->label, tree->right->truelabel, tree->right->falselabel);
        }
        else {
            addCode(tree, mystrcat("\tBeginCodeBlock\n", "\tEndCodeBlock\n"), NULL, NULL, NULL, NULL);
        }
        return;
    }
    
    else if (!strcmp(tree->token, "||")) {
        if(!strcmp(tree->left->token,"||")) calc3AC(tree->left);
        addCode(tree->right, NULL, NULL, NULL, NULL, NULL);
        addCode(tree->left, NULL, NULL, NULL, tree->falselabel, tree->falselabel);
        if(tree->right) calc3AC(tree->right);
        if(tree->left) calc3AC(tree->left);
        addCode(tree,mystrcat(tree->right->code, mystrcat(mystrcat("goto ", tree->right->truelabel), "\n")),NULL,NULL,NULL,NULL);
        addCode(tree, mystrcat(tree->code, mkspace(tree->right->falselabel)),NULL,NULL,NULL,NULL);
        addCode(tree, mystrcat(tree->code, tree->left->code),NULL,NULL,NULL,NULL);
    }
    
    else if (!strcmp(tree->token, "&&")) {
        addCode(tree->right, NULL, NULL, NULL, NULL, tree->falselabel);
        addCode(tree->left, NULL, NULL, NULL, NULL, tree->falselabel);
        if(tree->right) calc3AC(tree->right);
        if(tree->left) calc3AC(tree->left);
        addCode(tree, mystrcat(tree->code, tree->right->code),NULL,NULL,NULL,NULL);
        addCode(tree, mystrcat(tree->code, tree->left->code), NULL, NULL, NULL, NULL);
    }
    
    else {
        addCode(tree, NULL, freshVar(), NULL, NULL, NULL);
        if (!strcmp(tree->token, "&") || !strcmp(tree->token, "*")) {
            addCode(tree, mystrcat(tree->var, mystrcat(mystrcat(" = ", mystrcat(tree->token, tree->right->token)),"\n")), NULL, NULL, NULL, NULL);
        }
        else addCode(tree, gen(tree->var, "=", tree->token,"",""), NULL, NULL, NULL, NULL);
        if(tree->left) calc3AC(tree->left);
    }
    
}