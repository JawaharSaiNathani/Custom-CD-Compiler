%{
	#include <stdio.h>
	#include <string.h>
	#include <stdlib.h>
	
	void yyerror(char* s);
	int yylex();
	void ins();
	void insV();
	int flag=0;
	#define ANSI_COLOR_RED		"\x1b[31m"
	#define ANSI_COLOR_GREEN	"\x1b[32m"
	#define ANSI_COLOR_CYAN		"\x1b[36m"
	#define ANSI_COLOR_RESET	"\x1b[0m"
	extern char curid[20];
	extern char curtype[20];
	extern char curval[20];
	extern int currnest;
	void deletedata (int );
	int checkscope(char*);
	int check_id_is_func(char *);
	void insertST(char*, char*);
	void insertSTnest(char*, int);
	void insertSTparamscount(char*, int);
	int getSTparamscount(char*);
	int check_duplicate(char*);
	int check_declaration(char*, char *);
	int check_params(char*);
	int duplicate(char *s);
	int checkarray(char*);
	int check_main();
	int check_val_pos(char* s);
	int check_function(char* s);
	int findArray(char* s);
	char currfunctype[100];
	char currfunc[100];
	char currfunccall[100];
	void insertSTF(char*);
	char gettype(char*,int);
	char* getval(char*);
	char getfirst(char*);
	void push(char *s);
	void codegen();
	void codeassign();
	char* itoa(int num, char* str, int base);
	void reverse(char str[], int length); 
	void swap(char*,char*);
	void label1();
	void label2();
	void label3();
	void label4();
	void label5();
	void label6();
	void genunary();
	void codegencon();
	void funcgen();
	void funcgenend();
	void arggen();
	void callgen();
	void pr1(int num);
	void executeprint();
	void addfuncparam(char* s,char* prid);
	void returnstatement_id();
	void returnstatement_const();
	void initializeArray();
	void addtoArray();

	int params_count=0;
	int cur_pos = 0;
	int cur_v_pos = 0;
	int call_params_count=0;
	int top = 0,count=0,ltop=0,lno=0,cno=0,stor_char_count=0,function_count=0,getfunction_count=0,num_arrays=0,num_array_prints=0,isarrayiden=0,opp=0;
	int arrayprintindexes[20];
	char temp[3] = "t";
	char storChar[100][2];
	char storids[100][20];
	char intvar[100][20];
	char reserveretid[20];
	char storArraysize[20] = "";
	char arrayiden[20];

	FILE *fasmb;
%}

%nonassoc IF
%token INT CHAR FLOAT STRING
%token RETURN MAIN
%token VOID
%token WHILE FOR DO 
%token BREAK
%expect 1

%token identifier array_identifier func_identifier
%token integer_constant string_constant float_constant character_constant

%nonassoc ELSE

%right equal_operator

%left OR_operator
%left AND_operator
%left equality_operator inequality_operator
%left lessthan_equal_operator lessthan_operator greaterthan_equal_operator greaterthan_operator
%left add_operator subtract_operator
%left multiplication_operator division_operator modulo_operator

%right SIZEOF
%right exclamation_operator
%left increment_operator decrement_operator 


%start declaration_list

%%

declaration_list
			: declaration declaration_list 
			| declaration {if(check_main() == 0){yyerror("Function main not found");}};

declaration
			: declaration_variable
			| declaration_function ;

declaration_variable
			: type_specifier variable_declaration_list ';' ;

variable_declaration_list
			: variable_declaration_list ',' variable_declaration_identifier | variable_declaration_identifier;

variable_declaration_identifier 
			: identifier {push(curid);strcpy(reserveretid,curid);if(duplicate(curid)){printf("Variable %s already defined\n",curid);exit(0);}insertSTnest(curid,currnest); ins(); } identifier_value_declaration  { codeassign(); } 
			| array_identifier {if(duplicate(curid)){printf("Variable %s already defined\n",curid);exit(0);}insertSTnest(curid,currnest); ins();  } '[' array_declaration ;

identifier_value_declaration
			: equal_operator {push("=");} simple_expression
			| ;

array_declaration
			: integer_constant {strcpy(storArraysize,curval);}']' {initializeArray();} initilization {if($$ < 1) {printf("Wrong array size\n"); exit(0);} }
			| ']' string_initilization;

initilization
			: string_initilization
			| array_initialization
			| ;

array_initialization
			: equal_operator '{' array_int_declarations '}';

array_int_declarations
			: array_int_declarations ',' integer_constant {addtoArray();} | integer_constant {addtoArray();};

string_initilization
			: equal_operator string_constant {insV();} ;

type_specifier
			: INT | CHAR | FLOAT | STRING | VOID ;

declaration_function
			: function_declaration_type function_declaration_param_statement;

function_declaration_type
			: type_specifier identifier '('  { strcpy(currfunctype, curtype); strcpy(currfunc, curid); check_duplicate(curid); insertSTF(curid); ins(); };

function_declaration_param_statement
			: {params_count=0;}params ')' {funcgen();} statement {funcgenend();};

params 
			: parameters_list { insertSTparamscount(currfunc, params_count); }| { insertSTparamscount(currfunc, params_count); };

parameters_list 
			: type_specifier { check_params(curtype);} parameters_identifier_list ;

parameters_identifier_list 
			: param_identifier parameters_identifier_list_breakup;

parameters_identifier_list_breakup
			: ',' parameters_list 
			| ;

param_identifier 
			: identifier {addfuncparam(currfunc,curid); ins();insertSTnest(curid,1); params_count++; } param_identifier_breakup;

param_identifier_breakup
			: '[' ']'
			| ;

statement 
			: expression_statment | compound_statement 
			| conditional_statements | iterative_statements 
			| return_statement | break_statement 
			| declaration_variable;

compound_statement 
			: {currnest++;} '{'  statment_list  '}' {deletedata(currnest);currnest--;}  ;

statment_list 
			: statement statment_list 
			| ;

expression_statment 
			: expression ';' 
			| ';' ;

conditional_statements 
			: IF '(' simple_expression ')' {label1();if($3!=1){yyerror("Condition checking is not of type int\n");}} statement {label2();}  conditional_statements_breakup;

conditional_statements_breakup
			: ELSE statement {label3();}
			| {label3();};

iterative_statements 
			: WHILE '(' {label4();} simple_expression ')' {label1();if($4!=1){yyerror("Condition checking is not of type int\n");}} statement {label5();} 
			| FOR '(' expression ';' {label4();} simple_expression ';' {label1();if($6!=1){yyerror("Condition checking is not of type int\n");}} expression ')'statement {label5();} 
			| {label4();}DO statement WHILE '(' simple_expression ')'{label1();label5();if($6!=1){yyerror("Condition checking is not of type int\n");}} ';';
			
return_statement 
			: RETURN ';' {if(strcmp(currfunctype,"Void")) {yyerror("Returning void of a non-void function\n");}}
			| RETURN return_expression ';' { 	if(!strcmp(currfunctype, "Void"))
										{ 
											yyerror("Function is Void");
										}
									};

return_expression
			: identifier {returnstatement_id();}
			| integer_constant {returnstatement_const();} ;

break_statement 
			: BREAK ';' ;

expression 
			: mutable equal_operator {push("=");} expression   {   
																	  if($1==1 && $4==1) 
																	  {
			                                                          $$=1;
			                                                          } 
			                                                          else 
			                                                          {$$=-1; yyerror("Type mismatch\n");} 
			                                                          codeassign();
			                                                       }
			| mutable increment_operator 							{ push("++");if($1 == 1) $$=1; else $$=-1; genunary();}
			| mutable decrement_operator  							{push("--");if($1 == 1) $$=1; else $$=-1; genunary();}
			| simple_expression {if($1 == 1) $$=1; else $$=-1;} ;


simple_expression 
			: simple_expression OR_operator {push("||");} and_expression  {if($1 == 1 && $3==1) $$=1; else $$=-1; codegen();}
			| and_expression {if($1 == 1) $$=1; else $$=-1;};

and_expression 
			: and_expression AND_operator {push("&&");} unary_relation_expression  {if($1 == 1 && $3==1) $$=1; else $$=-1; codegen();}
			  |unary_relation_expression {if($1 == 1) $$=1; else $$=-1;} ;


unary_relation_expression 
			: exclamation_operator {push("!");} unary_relation_expression {if($2==1) $$=1; else $$=-1; codegen();} 
			| regular_expression {if($1 == 1) $$=1; else $$=-1;} ;

regular_expression 
			: regular_expression relational_operators sum_expression {if($1 == 1 && $3==1) $$=1; else $$=-1; codegen();}
			  | sum_expression {if($1 == 1) $$=1; else $$=-1;} ;
			
relational_operators 
			: greaterthan_equal_operator {push(">=");} | lessthan_equal_operator {push("<=");} | greaterthan_operator {push(">");}| lessthan_operator {push("<");}| equality_operator {push("==");}| inequality_operator {push("!=");} ;

sum_expression 
			: sum_expression sum_operators term  {if($1 == 1 && $3==1) $$=1; else $$=-1; codegen();}
			| term {if($1 == 1) $$=1; else $$=-1;};

sum_operators 
			: add_operator {push("+");}
			| subtract_operator {push("-");} ;

term
			: term MULOP factor {if($1 == 1 && $3==1) $$=1; else $$=-1; codegen();}			
			| factor {if($1 == 1) $$=1; else $$=-1;} ;

MULOP 
			: multiplication_operator {push("*");}| division_operator {push("/");} | modulo_operator {push("%");} ;

factor 
			: immutable {if($1 == 1) $$=1; else $$=-1;} 
			| mutable {if($1 == 1) $$=1; else $$=-1;} ;

mutable 
			: identifier {
						  push(curid);
						  if(check_id_is_func(curid))
						  {yyerror("Function name used as Identifier");}
			              if(!checkscope(curid))
			              {yyerror("Variable undeclared\n");} 
			              if(!checkarray(curid))
			              {printf("%s ",curid);printf("Invalid array identifier\n");exit(0);}
			              if(gettype(curid,0)=='I' || gettype(curid,1)== 'C')
			              $$ = 1;
			              else
			              $$ = -1;
			              }
			| array_identifier {push(curid);strcpy(arrayiden,curid);if(!checkscope(curid)){printf("Variable %s undeclared\n",curid);exit(0);}} '[' expression ']' 
			                   {
								  
								isarrayiden = 1;
								if(gettype(curid,0)=='I' || gettype(curid,1)== 'C')
			              		$$ = 1;
			              		else
			              		$$ = -1;
			              		};

immutable 
			: '(' expression ')' {if($2==1) $$=1; else $$=-1;}
			| call {if($1==-1) $$=-1; else $$=1;}
			| constant {if($1==1) $$=1; else $$=-1;};

call
			: identifier '('{
			             if(!check_declaration(curid, "Function"))
			             { yyerror("Function not declared");} 
			             insertSTF(curid); 
						 strcpy(currfunccall,curid);
						 if(gettype(curid,0)=='I' || gettype(curid,1)== 'C')
						 {
			             $$ = 1;
			             }
			             else
			             $$ = -1;
                         call_params_count=0;
			             } 
			             arguments { if(strcmp(currfunccall,"Print")==0){executeprint();} } ')' 
						 { if(strcmp(currfunccall,"Print") && strcmp(currfunccall,"Scan"))
							{ 
								if(getSTparamscount(currfunccall)!=call_params_count)
								{	
									yyerror("Number of arguments in function call doesn't match number of parameters");
									exit(8);
								}
							}
							callgen();
						 };

arguments 
			: arguments_list | ;

arguments_list 
			: arguments_list ',' exp { call_params_count++; }  
			| exp { call_params_count++; };

exp : array_identifier '[' integer_constant {sscanf(curval,"%d",&arrayprintindexes[num_array_prints]);num_array_prints++;} ']' {arggen(1);} |identifier {arggen(1);} | integer_constant {arggen(2);} | string_constant {arggen(3);} | float_constant {arggen(4);} | character_constant {arggen(5);} ;

constant 
			: integer_constant 	{  insV(); codegencon(); $$=1; } 
			| string_constant	{  insV(); codegencon();$$=-1;} 
			| float_constant	{  insV(); codegencon();} 
			| character_constant{  insV(); codegencon();$$=1; };

%%

extern FILE *yyin;
extern int yylineno;
extern char *yytext;
void insertSTtype(char *,char *);
void insertSTvalue(char *, char *);
void incertCT(char *, char *);
void printST();
void printCT();

struct stack
{
	char value[100];
	int labelvalue;
}s[100],label[100];

struct variable_position
{
	char name[100];
	int pos;
}var_pos[128];

struct function_params
{
	char name[100];
	char parameters[100][20];
	int parameters_count;
}fun_params[100];

struct storing_Array_Values
{
	char name[100];
	int size;
	int pos_start;
	int pos_end;
	int cur_pos;
}store_Arrays[20];

void initializeArray()
{
	if(num_arrays == 0)
	{
		strcpy(store_Arrays[0].name,curid);
		int tep;
		sscanf(storArraysize, "%d", &tep);
		store_Arrays[0].size = tep;
		store_Arrays[0].pos_start = 0;
		store_Arrays[0].pos_end = 8*store_Arrays[0].size;
		store_Arrays[0].cur_pos = 0;
	}
	else
	{
		int te = store_Arrays[num_arrays-1].pos_end;
		strcpy(store_Arrays[num_arrays].name,curid);
		int tep;
		sscanf(storArraysize, "%d", &tep);
		store_Arrays[num_arrays].size = tep;
		store_Arrays[num_arrays].pos_start = te;
		store_Arrays[num_arrays].pos_end = te + (8*store_Arrays[num_arrays].size);
		store_Arrays[num_arrays].cur_pos = 0;
	}
	num_arrays = num_arrays + 1;
}

int findArray(char* s)
{
	for(int i = 0; i < num_arrays ; i++)
	{
		if(strcmp(s,store_Arrays[i].name) == 0)
		{
			return i;
		}
	}
	return -1;
}

void addtoArray()
{
	int te = num_arrays - 1;
	fprintf(fasmb,"	mov rax, arrays\n");
	int tep = store_Arrays[te].cur_pos + store_Arrays[te].pos_start;
	store_Arrays[te].cur_pos  = store_Arrays[te].cur_pos + 8;
	fprintf(fasmb,"	add rax, %d\n",tep);
	fprintf(fasmb,"	mov rbx, %s\n",curval);
	fprintf(fasmb,"	mov [rax], rbx\n");
	if(tep >= store_Arrays[te].pos_end)
	{
		yyerror("Invalid Array Declaration.");
	}
}

int check_function(char* s)
{
	for(int i = 0 ; i < function_count ; i++ )
	{
		if(strcmp(fun_params[i].name,s)==0)
		{
			return i;
		}
	}
	return -1;
}

void addfuncparam(char* s, char* prid)
{
	int te = check_function(s);
	if(te == -1)
	{
		strcpy(fun_params[function_count].name,s);
		strcpy(fun_params[function_count].parameters[0],prid);
		fun_params[function_count].parameters_count = 1;
		function_count = function_count + 1;
	}
	else
	{
		strcpy(fun_params[te].parameters[fun_params[te].parameters_count],prid);
		fun_params[te].parameters_count = fun_params[te].parameters_count + 1;
	}
}

void returnstatement_id()
{
	int te = check_val_pos(curid);
	if(te == -1)
	{
		printf("Variable Undeclared\n");
		exit(1);
	}
	fprintf(fasmb,"	mov rax, result\n");
	fprintf(fasmb,"	mov rbx, stor\n");
	fprintf(fasmb,"	add rbx, %d\n",var_pos[te].pos);
	fprintf(fasmb,"	mov rbx, [rbx]\n");
	fprintf(fasmb,"	mov [rax], rbx\n\n");
	fprintf(fasmb,"	ret\n\n");
}

void returnstatement_const()
{
	fprintf(fasmb,"	mov rax, result\n");
	fprintf(fasmb,"	mov rbx, %s\n",curval);
	fprintf(fasmb,"	mov [rax], rbx\n\n");
	fprintf(fasmb,"	ret\n\n");
}

void push(char *x)
{
	strcpy(s[++top].value,x);
}
    
void swap(char *x, char *y)
{
	char temp = *x;
	*x = *y;
	*y = temp;
}

void reverse(char str[], int length) 
{ 
    int start = 0; 
    int end = length -1; 
    while (start < end) 
    { 
        swap((str+start), (str+end)); 
        start++; 
        end--; 
    } 
} 

char* itoa(int num, char* str, int base) 
{ 
    int i = 0; 
    int isNegative = 0; 
  
   
    if (num == 0) 
    { 
        str[i++] = '0'; 
        str[i] = '\0'; 
        return str; 
    } 
  
    if (num < 0 && base == 10) 
    { 
        isNegative = 1; 
        num = -num; 
    } 
  
   
    while (num != 0) 
    { 
        int rem = num % base; 
        str[i++] = (rem > 9)? (rem-10) + 'a' : rem + '0'; 
        num = num/base; 
    } 
  
    if (isNegative) 
        str[i++] = '-'; 
  
    str[i] = '\0'; 
  
   
    reverse(str, i); 
  
    return str; 
} 

int check_val_pos(char* s)
{
	for(int i = 0 ; i < cur_v_pos ; i++ )
	{
		if(strcmp(s,var_pos[i].name) == 0)
		{
			return i;
		}
	}
	return -1;
}

void pr1(int num)
{
	if(num == -1)
	{
		strcpy(var_pos[cur_v_pos].name,temp);
		if(cur_v_pos == 0)
		{
			var_pos[cur_v_pos].pos = 0;
			fprintf(fasmb,"	mov rax, stor\n");
			fprintf(fasmb,"	add rax, 0\n");
			fprintf(fasmb,"	mov [rax], rbx\n\n");
		}
		else{
			var_pos[cur_v_pos].pos = var_pos[cur_v_pos-1].pos + 8;
			fprintf(fasmb,"	mov rax, stor\n");
			fprintf(fasmb,"	add rax, %d\n",var_pos[cur_v_pos].pos);
			fprintf(fasmb,"	mov [rax], rbx\n\n");
		}
		cur_v_pos = cur_v_pos + 1;
	}
	else{
		fprintf(fasmb,"	mov rax, stor\n");
		fprintf(fasmb,"	add rax, %d\n",var_pos[num].pos);
		fprintf(fasmb,"	mov [rax], rbx\n\n");
	}
}

void codegen()
{
	strcpy(temp,"t");
	char buffer[100];
	itoa(count,buffer,10);
	strcat(temp,buffer);
	int tep = 0;
	if(isarrayiden == 1)
	{
		isarrayiden = 0;
		int t3 = findArray(arrayiden);
		int t2 = check_val_pos(s[top-2].value);
		int t1 = check_val_pos(s[top].value);
		fprintf(fasmb,"	mov rax, stor\n");
		fprintf(fasmb,"	add rax, %d\n",var_pos[t2].pos);
		fprintf(fasmb,"	mov rax, [rax]\n");
		fprintf(fasmb,"	mov rbx, 8\n");
		fprintf(fasmb,"	mul rbx\n");
		fprintf(fasmb,"	mov rbx, rax\n");
		fprintf(fasmb,"	mov rax, arrays\n");
		fprintf(fasmb,"	add rax, %d\n",store_Arrays[t3].pos_start);
		fprintf(fasmb,"	add rax, rbx\n");
		fprintf(fasmb,"	mov rbx, [rax]\n");
		fprintf(fasmb,"	mov rax, stor\n");
		fprintf(fasmb,"	add rax, %d\n",var_pos[t1].pos);
		fprintf(fasmb,"	mov rcx, [rax]\n");
		
		tep = 1;
	}
	else
	{
		int t1 = check_val_pos(s[top-2].value);
		int t2 = check_val_pos(s[top].value);
		fprintf(fasmb,"	mov rax, stor\n");
		fprintf(fasmb,"	add rax, %d\n",var_pos[t1].pos);
		fprintf(fasmb,"	mov rbx, [rax]\n");
		fprintf(fasmb,"	mov rax, stor\n");
		fprintf(fasmb,"	add rax, %d\n",var_pos[t2].pos);
		fprintf(fasmb,"	mov rcx, [rax]\n");
	}
	if(strcmp(s[top-1].value,"+")==0)
	{
		fprintf(fasmb,"	add rbx, rcx\n");
	}
	else if(strcmp(s[top-1].value,"-")==0)
	{
		fprintf(fasmb,"	sub rbx, rcx\n");
	}
	else if(strcmp(s[top-1].value,"*")==0)
	{
		fprintf(fasmb,"	mov rax, rcx\n");
		fprintf(fasmb,"	mul rbx\n");
		fprintf(fasmb,"	mov rbx, rax\n");
	}
	else if(strcmp(s[top-1].value,"/")==0)
	{
		fprintf(fasmb,"	mov rax, rbx\n");
		fprintf(fasmb,"	div rcx\n");
		fprintf(fasmb,"	mov rbx, rax\n");
	}
	else if(strcmp(s[top-1].value,"%")==0)
	{
		fprintf(fasmb,"	mov rax, rbx\n");
		fprintf(fasmb,"	div rcx\n");
		fprintf(fasmb,"	mov rbx, rdx\n");
	}
	else if(strcmp(s[top-1].value,"&&")==0)
	{
		char buffer1[100];
		char com1[100];
		char com2[100];
		char com3[100];
		itoa(opp,buffer1,10);
		strcpy(com1,"_accept");
		strcpy(com2,"_denied");
		strcpy(com3,"_End");
		strcat(com1,buffer1);
		strcat(com2,buffer1);
		strcat(com3,buffer1);
		opp = opp + 1;

		fprintf(fasmb,"	and rbx, rcx\n");
		fprintf(fasmb,"	cmp rbx, 0\n");
		fprintf(fasmb,"	jne %s\n",com1);
		fprintf(fasmb,"	jmp %s\n\n",com2);
		fprintf(fasmb,"%s:\n",com1);
		fprintf(fasmb,"	mov rbx, 1\n");
		fprintf(fasmb,"	jmp %s\n\n",com3);
		fprintf(fasmb,"%s:\n",com2);
		fprintf(fasmb,"	mov rbx, 0\n");
		fprintf(fasmb,"	jmp %s\n\n",com3);
		fprintf(fasmb,"%s:\n",com3);
	}
	else if(strcmp(s[top-1].value,"||")==0)
	{
		char buffer1[100];
		char com1[100];
		char com2[100];
		char com3[100];
		itoa(opp,buffer1,10);
		strcpy(com1,"_accept");
		strcpy(com2,"_denied");
		strcpy(com3,"_End");
		strcat(com1,buffer1);
		strcat(com2,buffer1);
		strcat(com3,buffer1);
		opp = opp + 1;

		fprintf(fasmb,"	or rbx, rcx\n");
		fprintf(fasmb,"	cmp rbx, 0\n");
		fprintf(fasmb,"	jne %s\n",com1);
		fprintf(fasmb,"	jmp %s\n\n",com2);
		fprintf(fasmb,"%s:\n",com1);
		fprintf(fasmb,"	mov rbx, 1\n");
		fprintf(fasmb,"	jmp %s\n\n",com3);
		fprintf(fasmb,"%s:\n",com2);
		fprintf(fasmb,"	mov rbx, 0\n");
		fprintf(fasmb,"	jmp %s\n\n",com3);
		fprintf(fasmb,"%s:\n",com3);
	}
	else
	{
		char buffer1[100];
		char com1[100];
		char com2[100];
		char com3[100];
		itoa(cno,buffer1,10);
		strcpy(com1,"_P");
		strcpy(com2,"_Q");
		strcpy(com3,"_E");
		strcat(com1,buffer1);
		strcat(com2,buffer1);
		strcat(com3,buffer1);

		fprintf(fasmb,"	cmp rbx, rcx\n");

		if(strcmp(s[top-1].value,"<")==0)
		{
			fprintf(fasmb,"	jl %s\n",com1);
		}
		else if(strcmp(s[top-1].value,"<=")==0)
		{
			fprintf(fasmb,"	jle %s\n",com1);
		}
		else if(strcmp(s[top-1].value,">=")==0)
		{
			fprintf(fasmb,"	jge %s\n",com1);
		}
		else if(strcmp(s[top-1].value,">")==0)
		{
			fprintf(fasmb,"	jg %s\n",com1);
		}
		else if(strcmp(s[top-1].value,"==")==0)
		{
			fprintf(fasmb,"	je %s\n",com1);
		}
		else if(strcmp(s[top-1].value,"!=")==0)
		{
			fprintf(fasmb,"	jne %s\n",com1);
		}

		fprintf(fasmb,"	jmp %s\n",com2);
		fprintf(fasmb,"%s:\n",com1);
		fprintf(fasmb,"	mov rbx, 1\n");
		fprintf(fasmb,"	jmp %s\n",com3);
		fprintf(fasmb,"%s:\n",com2);
		fprintf(fasmb,"	mov rbx, 0\n");
		fprintf(fasmb,"	jmp %s\n",com3);
		fprintf(fasmb,"%s:\n",com3);
		cno = cno + 1;
	}
	int t3 = check_val_pos(temp);
	pr1(t3);
	top = top - 2 - tep;
	strcpy(s[top].value,temp);
	count++; 
}

void codegencon()
{
	strcpy(temp,"t");
	char buffer[100];
	itoa(count,buffer,10);
	strcat(temp,buffer);
	push(temp);
	count++;
	int te = check_val_pos(temp);
	if(te == -1)
	{
		strcpy(var_pos[cur_v_pos].name,temp);
		if(cur_v_pos == 0)
		{
			var_pos[cur_v_pos].pos = 0;
			fprintf(fasmb,"	mov rax, stor\n");
			fprintf(fasmb,"	add rax, 0\n");
			fprintf(fasmb,"	mov rbx, %s\n",curval);
			fprintf(fasmb,"	mov [rax], rbx\n\n");
		}
		else{
			var_pos[cur_v_pos].pos = var_pos[cur_v_pos-1].pos + 8;
			fprintf(fasmb,"	mov rax, stor\n");
			fprintf(fasmb,"	add rax, %d\n",var_pos[cur_v_pos].pos);
			fprintf(fasmb,"	mov rbx, %s\n",curval);
			fprintf(fasmb,"	mov [rax], rbx\n\n");
		}
		cur_v_pos = cur_v_pos + 1;
	}
	else{
		fprintf(fasmb,"	mov rax, stor\n");
		fprintf(fasmb,"	add rax, %d\n",var_pos[te].pos);
		fprintf(fasmb,"	mov rbx, %s\n",curval);
		fprintf(fasmb,"	mov [rax], rbx\n\n");
	}
}

int isunary(char *s)
{
	if(strcmp(s, "--")==0 || strcmp(s, "++")==0)
	{
		return 1;
	}
	return 0;
}

void genunary()
{
	char temp1[100], temp2[100], temp3[100];
	strcpy(temp1, s[top].value);
	strcpy(temp2, s[top-1].value);

	if(isunary(temp1))
	{
		strcpy(temp3, temp1);
		strcpy(temp1, temp2);
		strcpy(temp2, temp3);
	}
	strcpy(temp, "t");
	char buffer[100];
	itoa(count, buffer, 10);
	strcat(temp, buffer);
	count++;

	if(strcmp(temp2,"--")==0)
	{
		int te = check_val_pos(temp1);
		fprintf(fasmb,"	mov rax, stor\n");
		fprintf(fasmb,"	add rax, %d\n",var_pos[te].pos);
		fprintf(fasmb,"	mov rbx, [rax]\n");
		fprintf(fasmb,"	sub rbx, 1\n");
		fprintf(fasmb,"	mov [rax], rbx\n\n");
	}

	if(strcmp(temp2,"++")==0)
	{
		int te = check_val_pos(temp1);
		fprintf(fasmb,"	mov rax, stor\n");
		fprintf(fasmb,"	add rax, %d\n",var_pos[te].pos);
		fprintf(fasmb,"	mov rbx, [rax]\n");
		fprintf(fasmb,"	add rbx, 1\n");
		fprintf(fasmb,"	mov [rax], rbx\n\n");
	}

	top = top -2;
}

void codeassign()
{
	if(isarrayiden == 1)
	{
		isarrayiden = 0;
	}
	if(checkarray(s[top-3].value) == 0)
	{
		int t3 = findArray(s[top-3].value);
		int t2 = check_val_pos(s[top-2].value);
		int t1 = check_val_pos(s[top].value);
		fprintf(fasmb,"	mov rax, stor\n");
		fprintf(fasmb,"	add rax, %d\n",var_pos[t2].pos);
		fprintf(fasmb,"	mov rax, [rax]\n");
		fprintf(fasmb,"	mov rbx, 8\n");
		fprintf(fasmb,"	mul rbx\n");
		fprintf(fasmb,"	mov rbx, rax\n");
		fprintf(fasmb,"	mov rax, stor\n");
		fprintf(fasmb,"	add rax, %d\n",var_pos[t1].pos);
		fprintf(fasmb,"	mov rcx, [rax]\n");
		fprintf(fasmb,"	mov rax, arrays\n");
		fprintf(fasmb,"	add rax, %d\n",store_Arrays[t3].pos_start);
		fprintf(fasmb,"	add rax, rbx\n");
		fprintf(fasmb,"	mov [rax], rcx\n\n");

		top = top - 3;
	}
	else
	{
		if(strcmp(s[top].value,"result") != 0)
		{
			int t1 = check_val_pos(s[top].value);
			int t2 = check_val_pos(s[top-2].value);
			if(t2 == -1)
			{
				strcpy(var_pos[cur_v_pos].name,s[top-2].value);
				var_pos[cur_v_pos].pos = var_pos[t1].pos;
				cur_v_pos = cur_v_pos + 1;
			}
			else
			{
				fprintf(fasmb,"	mov rax, stor\n");
				fprintf(fasmb,"	add rax, %d\n",var_pos[t1].pos);
				fprintf(fasmb,"	mov rbx, [rax]\n");
				fprintf(fasmb,"	mov rax, stor\n");
				fprintf(fasmb,"	add rax, %d\n",var_pos[t2].pos);
				fprintf(fasmb,"	mov [rax], rbx\n\n");
			}
			top = top - 2;
		}
	}
}

void label1()
{
	strcpy(temp,"_L");
	char buffer[100];
	itoa(lno,buffer,10);
	strcat(temp,buffer);
	int te = check_val_pos(s[top].value);
	fprintf(fasmb,"	mov rax, stor\n");
	fprintf(fasmb,"	add rax, %d\n",var_pos[te].pos);
	fprintf(fasmb,"	mov rbx, [rax]\n");
	fprintf(fasmb,"	cmp rbx, 1\n");
	fprintf(fasmb,"	jne %s\n\n",temp);
	label[++ltop].labelvalue = lno++;
}

void label2()
{
	strcpy(temp,"_L");
	char buffer[100];
	itoa(lno,buffer,10);
	strcat(temp,buffer);
	fprintf(fasmb,"	jmp %s\n",temp);
	strcpy(temp,"_L");
	itoa(label[ltop].labelvalue,buffer,10);
	strcat(temp,buffer);
	fprintf(fasmb,"%s:\n",temp);
	ltop--;
	label[++ltop].labelvalue=lno++;
}

void label3()
{
	strcpy(temp,"_L");
	char buffer[100];
	itoa(label[ltop].labelvalue,buffer,10);
	strcat(temp,buffer);
	fprintf(fasmb,"%s:\n",temp);
	ltop--;
	
}

void label4()
{
	strcpy(temp,"_L");
	char buffer[100];
	itoa(lno,buffer,10);
	strcat(temp,buffer);
	fprintf(fasmb,"%s:\n",temp);
	label[++ltop].labelvalue = lno++;
}


void label5()
{
	strcpy(temp,"_L");
	char buffer[100];
	itoa(label[ltop-1].labelvalue,buffer,10);
	strcat(temp,buffer);
	fprintf(fasmb,"	jmp %s\n",temp);
	strcpy(temp,"_L");
	itoa(label[ltop].labelvalue,buffer,10);
	strcat(temp,buffer);
	fprintf(fasmb,"%s:\n",temp);
	ltop = ltop - 2;
}

void funcgen()
{
	if(strcmp(currfunc,"Print") != 0 && strcmp(currfunc,"Scan") != 0)
	{
		fprintf(fasmb,"_%s:\n",currfunc);	
	}
	if(strcmp(currfunc,"main") != 0 && strcmp(currfunc,"Print") != 0 && strcmp(currfunc,"Scan") != 0)
	{
		for(int i=0;i<fun_params[function_count-1].parameters_count;i++)
		{
			int te = check_val_pos(fun_params[function_count-1].parameters[i]);
			if(te == -1)
			{
				strcpy(var_pos[cur_v_pos].name,fun_params[function_count-1].parameters[i]);
				if(cur_v_pos == 0)
				{
					var_pos[cur_v_pos].pos = 0;
				}
				else{
					var_pos[cur_v_pos].pos = var_pos[cur_v_pos-1].pos + 8;
				}
				cur_v_pos = cur_v_pos + 1;
			}
		}	
	}
}

void executeprint()
{
	int tem = 0;
	char str_tem[1024] = "";
	int intvar_count = 0;
	int accintvar_count = 0;
	for(int i=0;i<stor_char_count;i++)
	{
		if(storChar[i][0] == '%' && (storChar[i][1] == 'I' || storChar[i][1] == 'S' || storChar[i][1] == 'C'))
		{
			if(gettype(storids[tem],0) == '\0')
			{
				yyerror("Invalid Print Statement => check parameters");
				exit(1);
			}
			else
			{
				if(gettype(storids[tem],0) == storChar[i][1])
				{
					if(storChar[i][1] == 'I')
					{
						strcpy(intvar[intvar_count],storids[tem]);
						intvar_count++;
						strcat(str_tem,"`");
					}
					else
					{
						char* idval = getval(storids[tem]);
						char idval1[strlen(idval)-2];
						for(int i = 1 ; i < strlen(idval) - 1 ; i++ )
						{
							idval1[i-1] = idval[i];
						}
						
						for(int i = strlen(idval)-2;i<strlen(idval1);i++)
						{
							idval1[i] = '\0';
						}
						strcat(str_tem,idval1);
					}
				}
				else{
					yyerror("Type Mismatch");
					exit(1);
				}
			}
			tem = tem + 1;
		}
		else if(storChar[i][0] == '\\' && storChar[i][1] == 'n')
		{
			strcat(str_tem,"~");
		}
		else{
			strcat(str_tem,storChar[i]);
		}
		storChar[i][0] = '\0';
		storChar[i][1] = '\0';
		
	}
	if(tem != call_params_count-1)
	{
		yyerror("Invalid Print Statement => check parameters");
		exit(1);
	}
	tem = 0;
	char* t4;
	strcpy(t4,"\"");
	int t5 = 0;
	int count_num_print_arrays = 0;
	for(int i = 0 ; i < strlen(str_tem) ; i++ )
	{
		if(str_tem[i] == '~')
		{
			while(t5<strlen(t4))
			{
				t4[t5+1] = '\0';
				t5++;
			}
			strcat(t4,"\"");
			fprintf(fasmb,"	mov rax, name\n");
			fprintf(fasmb,"	mov rbx, %s\n",t4);
			fprintf(fasmb,"	mov [rax], rbx\n");
			fprintf(fasmb,"	mov rax, name\n");
			fprintf(fasmb,"	call _print\n\n");

			fprintf(fasmb,"	mov rax, name\n");
			fprintf(fasmb,"	mov rbx, 10\n");
			fprintf(fasmb,"	mov [rax], rbx\n");
			fprintf(fasmb,"	mov rax, name\n");
			fprintf(fasmb,"	call _print\n\n");

			strcpy(t4,"\"");
			t5 = 0;
		}
		else if(str_tem[i] == '`')
		{
			while(t5<strlen(t4))
			{
				t4[t5+1] = '\0';
				t5++;
			}
			strcat(t4,"\"");
			fprintf(fasmb,"	mov rax, name\n");
			fprintf(fasmb,"	mov rbx, %s\n",t4);
			fprintf(fasmb,"	mov [rax], rbx\n");
			fprintf(fasmb,"	mov rax, name\n");
			fprintf(fasmb,"	call _print\n\n");

			if(checkarray(intvar[accintvar_count]) == 0)
			{
				int tep = arrayprintindexes[count_num_print_arrays];
				int tep1 = findArray(intvar[accintvar_count]);
				count_num_print_arrays = count_num_print_arrays + 1;
				accintvar_count++;
				fprintf(fasmb,"	mov rax, %d\n",tep);
				fprintf(fasmb,"	mov rbx, 8\n");
				fprintf(fasmb,"	mul rbx\n");
				fprintf(fasmb,"	mov rbx, rax\n");
				fprintf(fasmb,"	mov rax, arrays\n");
				fprintf(fasmb,"	add rax, %d\n",store_Arrays[tep1].pos_start);
				fprintf(fasmb,"	add rax, rbx\n");
				fprintf(fasmb,"	mov rax, [rax]\n");
				fprintf(fasmb,"	call _printRAX\n");
			}
			else
			{
				int t6 = check_val_pos(intvar[accintvar_count]);
				accintvar_count++;
				fprintf(fasmb,"	mov rax, stor\n");
				fprintf(fasmb,"	add rax, %d\n",var_pos[t6].pos);
				fprintf(fasmb,"	mov rax, [rax]\n");
				fprintf(fasmb,"	call _printRAX\n\n");
			}

			strcpy(t4,"\"");
			t5 = 0;
		}
		else{
			t4[t5+1] = str_tem[i];
			t5 = t5 + 1;
		}
		if(t5 == 8)
		{
			while(t5<strlen(t4))
			{
				t4[t5+1] = '\0';
				t5++;
			}
			strcat(t4,"\"");
			fprintf(fasmb,"	mov rax, name\n");
			fprintf(fasmb,"	mov rbx, %s\n",t4);
			fprintf(fasmb,"	mov [rax], rbx\n");
			fprintf(fasmb,"	mov rax, name\n");
			fprintf(fasmb,"	call _print\n\n");
			t5 = 0;
			strcpy(t4,"\"");
		}
	}
	num_array_prints = 0;
	while(t5<strlen(t4))
	{
		t4[t5+1] = '\0';
		t5++;
	}
	strcat(t4,"\"");
	fprintf(fasmb,"	mov rax, name\n");
	fprintf(fasmb,"	mov rbx, %s\n",t4);
	fprintf(fasmb,"	mov [rax], rbx\n");
	fprintf(fasmb,"	mov rax, name\n");
	fprintf(fasmb,"	call _print\n\n");
	t5 = 0;
	strcpy(t4,"\"");
}

void funcgenend()
{
	if(strcmp(currfunc,"main") == 0)
	{
		fprintf(fasmb,"	mov rax, 60\n");
		fprintf(fasmb,"	mov rdi, 0\n");
		fprintf(fasmb,"	syscall\n\n");
	}
	else
	{
		fprintf(fasmb,"	ret\n\n");	
	}
}

void arggen(int i)
{
    if(i==1 && strcmp(currfunccall,"Print")==0)
    {
		strcpy(storids[call_params_count-1],curid);
	}
	else if(i==3 && strcmp(currfunccall,"Print")==0)
	{
		int cvsize = strlen(curval);
		int flag = 0;
		stor_char_count = 0;
		int i = 1;
		while( i < cvsize )
		{
			
			if(flag == 1)
			{
				printf("Invalid Print Statement\n");
				exit(1);
			}
			else if(curval[i] == '"')
			{
				flag = 1;
			}
			else if(curval[i] == '%' || (curval[i] == '\\' && curval[i+1] == 'n'))
			{
				storChar[stor_char_count][0] = curval[i];
				storChar[stor_char_count][1] = curval[i+1];
				stor_char_count = stor_char_count + 1;
				i = i + 1;
			}
			else
			{
				storChar[stor_char_count][0] = curval[i];
				stor_char_count = stor_char_count + 1;
			}
			i = i + 1;
		}
	}
	else{
		if(strcmp(currfunccall,"Print")==0)
		{ 
			yyerror("Invalid Print Statement");
			exit(1);
		}
		else if(strcmp(currfunccall,"Scan")==0)
		{
			if(i == 1)
			{
				int te = check_val_pos(curid);
				if(te == -1)
				{
					yyerror("Variables undeclared");
					exit(1);
				}
				else
				{
					if(gettype(curid,0) == 'I')
					{
						fprintf(fasmb,"	call _getInput\n\n");

						fprintf(fasmb,"	xor rax, rax\n");
						fprintf(fasmb,"	mov rdx, scanned\n");
						fprintf(fasmb,"	call _convert\n\n");

						fprintf(fasmb,"	mov rax, stor\n");
						fprintf(fasmb,"	add rax, %d\n\n",var_pos[te].pos);
						fprintf(fasmb,"	mov rbx, converted\n");
						fprintf(fasmb,"	mov rbx, [rbx]\n");
						fprintf(fasmb,"	mov [rax], rbx\n\n");
					}
					else{
						yyerror("SCAN only supports INTEGERS");
					}
				}
			}
			else{
				yyerror("Invalid Scan Statement");
			}
		}
		else
		{
			if(i == 1)
			{
				int ta = check_val_pos(curid);
				int fun_num = check_function(currfunccall);
				int te = check_val_pos(fun_params[fun_num].parameters[getfunction_count]);
				getfunction_count = getfunction_count + 1;
				fprintf(fasmb,"	mov rax, stor\n");
				fprintf(fasmb,"	add rax, %d\n",var_pos[ta].pos);
				fprintf(fasmb,"	mov rax, [rax]\n");
				fprintf(fasmb,"	mov rbx, stor\n");
				fprintf(fasmb,"	add rbx, %d\n",var_pos[te].pos);
				fprintf(fasmb,"	mov [rbx], rax\n\n");
				if(getfunction_count == fun_params[fun_num].parameters_count)
				{
					getfunction_count = 0;
				}
			}
			else if(i == 2)
			{
				int fun_num = check_function(currfunccall);
				int te = check_val_pos(fun_params[fun_num].parameters[getfunction_count]);
				getfunction_count = getfunction_count + 1;
				fprintf(fasmb,"	mov rbx, stor\n");
				fprintf(fasmb,"	add, rbx, %d\n",var_pos[te].pos);
				fprintf(fasmb,"	mov [rbx], %s\n\n",curval);
				if(getfunction_count == fun_params[fun_num].parameters_count)
				{
					getfunction_count = 0;
				}
			}
			else{
				yyerror("Invalid Function call");
				exit(1);
			}
		}
	}
}

void callgen()
{
	push("result");
	if(strcmp(currfunccall,"main") != 0 && strcmp(currfunccall,"Print") != 0 && strcmp(currfunccall,"Scan") != 0)
	{
		fprintf(fasmb,"	call _%s\n",currfunccall);
		if(gettype(currfunccall,0) == 'I' && gettype(reserveretid,0) == 'I')
		{
			int te = check_val_pos(reserveretid);
			if(te == -1)
			{
				strcpy(var_pos[cur_v_pos].name,reserveretid);
				if(cur_v_pos == 0)
				{
					var_pos[cur_v_pos].pos = 0;
					fprintf(fasmb,"	mov rax, result\n");
					fprintf(fasmb,"	mov rax, [rax]\n");
					fprintf(fasmb,"	mov rbx, stor\n");
					fprintf(fasmb,"	add rbx, 0\n");
					fprintf(fasmb,"	mov [rbx], rax\n\n");
				}
				else
				{
					var_pos[cur_v_pos].pos = var_pos[cur_v_pos-1].pos + 8;
					fprintf(fasmb,"	mov rax, result\n");
					fprintf(fasmb,"	mov rax, [rax]\n");
					fprintf(fasmb,"	mov rbx, stor\n");
					fprintf(fasmb,"	add rbx, %d\n",var_pos[cur_v_pos].pos);
					fprintf(fasmb,"	mov [rbx], rax\n\n");
				}
				cur_v_pos = cur_v_pos + 1;
			}
			else
			{
				fprintf(fasmb,"	mov rax, result\n");
				fprintf(fasmb,"	mov rax, [rax]\n");
				fprintf(fasmb,"	mov rbx, stor\n");
				fprintf(fasmb,"	add rbx, %d\n",var_pos[te].pos);
				fprintf(fasmb,"	mov [rbx], rax\n\n");
			}
		}
	}
}

int main(int argc , char **argv)
{
	fasmb = fopen("output.asm","w");

	fprintf(fasmb,"section .bss\n");
	fprintf(fasmb,"	digitSpace resb 100\n");
	fprintf(fasmb,"	digitSpacePos resb 8\n");
	fprintf(fasmb,"	stor resb 1024\n");
	fprintf(fasmb,"	arrays resb 1024\n");
	fprintf(fasmb,"	name resb 100\n");
	fprintf(fasmb,"	scanned resb 100\n");
	fprintf(fasmb,"	converted resb 8\n");
	fprintf(fasmb,"	result resb 8\n\n");
	
	fprintf(fasmb,"section .data\n\n");
	
	fprintf(fasmb,"section .text\n");
	fprintf(fasmb,"	global _start\n\n");
	
	fprintf(fasmb,"_start:\n");
	fprintf(fasmb,"	jmp _main\n");


	yyin = fopen(argv[1], "r");
	yyparse();
	

	fprintf(fasmb,"_getInput:\n");
    fprintf(fasmb,"	mov rax, 0\n");
    fprintf(fasmb,"	mov rdi, 0\n");
    fprintf(fasmb,"	mov rsi, scanned\n");
    fprintf(fasmb,"	mov rdx, 100\n");
    fprintf(fasmb,"	syscall\n");
    fprintf(fasmb,"	ret\n\n");

	fprintf(fasmb,"_convert:\n");
	fprintf(fasmb,"	movzx rcx, byte [rdx]\n");
	fprintf(fasmb,"	inc rdx\n");
	fprintf(fasmb,"	cmp rcx, '0'\n");
	fprintf(fasmb,"	jb _doneconverting\n");
	fprintf(fasmb,"	cmp rcx, '9'\n");
	fprintf(fasmb,"	ja _doneconverting\n");
	fprintf(fasmb,"	sub rcx, '0'\n");
	fprintf(fasmb,"	imul rax, 10\n");
	fprintf(fasmb,"	add rax, rcx\n");
	fprintf(fasmb,"	jmp _convert\n\n");

	fprintf(fasmb,"_doneconverting:\n");
	fprintf(fasmb,"	mov rbx, converted\n");
	fprintf(fasmb,"	mov [rbx], rax\n");
	fprintf(fasmb,"	ret\n\n");

	fprintf(fasmb,"_print:\n");
    fprintf(fasmb,"	push rax\n");
    fprintf(fasmb,"	mov rbx, 0\n\n");

	fprintf(fasmb,"_printloop:\n");
    fprintf(fasmb,"	inc rax\n");
    fprintf(fasmb,"	inc rbx\n");
    fprintf(fasmb,"	mov cl, [rax]\n");
    fprintf(fasmb,"	cmp cl, 0\n");
    fprintf(fasmb,"	jne _printloop\n\n");

    fprintf(fasmb,"	mov rax, 1\n");
    fprintf(fasmb,"	mov rdi, 1\n");
    fprintf(fasmb,"	pop rsi\n");
    fprintf(fasmb,"	mov rdx, rbx\n");
    fprintf(fasmb,"	syscall\n\n");
	fprintf(fasmb,"	ret\n\n");

	fprintf(fasmb,"_printRAX:\n");
	fprintf(fasmb,"    mov rcx, digitSpace\n");
	fprintf(fasmb,"    mov rbx, 0\n");
	fprintf(fasmb,"    mov [rcx], rbx\n");
	fprintf(fasmb,"    inc rcx\n");
	fprintf(fasmb,"    mov [digitSpacePos], rcx\n\n");
 
	fprintf(fasmb,"_printRAXLoop:\n");
	fprintf(fasmb,"    mov rdx, 0\n");
	fprintf(fasmb,"    mov rbx, 10\n");
	fprintf(fasmb,"    div rbx\n");
	fprintf(fasmb,"    push rax\n");
	fprintf(fasmb,"    add rdx, 48\n\n");
 
	fprintf(fasmb,"    mov rcx, [digitSpacePos]\n");
	fprintf(fasmb,"    mov [rcx], dl\n");
	fprintf(fasmb,"    inc rcx\n");
	fprintf(fasmb,"    mov [digitSpacePos], rcx\n\n");
    
	fprintf(fasmb,"    pop rax\n");
	fprintf(fasmb,"    cmp rax, 0\n");
	fprintf(fasmb,"    jne _printRAXLoop\n\n");
 
	fprintf(fasmb,"_printRAXLoop2:\n");
	fprintf(fasmb,"    mov rcx, [digitSpacePos]\n\n");
 
	fprintf(fasmb,"    mov rax, 1\n");
	fprintf(fasmb,"    mov rdi, 1\n");
	fprintf(fasmb,"    mov rsi, rcx\n");
	fprintf(fasmb,"    mov rdx, 1\n");
	fprintf(fasmb,"    syscall\n\n");
 
	fprintf(fasmb,"    mov rcx, [digitSpacePos]\n");
	fprintf(fasmb,"    dec rcx\n");
	fprintf(fasmb,"    mov [digitSpacePos], rcx\n\n");
 
	fprintf(fasmb,"    cmp rcx, digitSpace\n");
	fprintf(fasmb,"    jge _printRAXLoop2\n\n");
 
	fprintf(fasmb,"    ret\n\n");

	fclose(fasmb);
}

void yyerror(char *s)
{
	printf(ANSI_COLOR_RED "Line no %d: %s >> %s\n", yylineno+1, s, yytext);
	flag=1;
	printf(ANSI_COLOR_RED "Status: Parsing Failed - Invalid\n" ANSI_COLOR_RESET);
	exit(7);
}

void ins()
{
	insertSTtype(curid,curtype);
}

void insV()
{
	insertSTvalue(curid,curval);
}

int yywrap()
{
	return 1;
}
