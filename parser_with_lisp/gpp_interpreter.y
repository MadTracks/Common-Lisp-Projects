%{
#include <stdio.h>
#include <stdlib.h>
void yyerror(char* s);
int yylex();
%}


%union {int value; 
struct{ 
char name[256]; 
int value; 
}identi; 
int * list;}

%start INPUT
%token OP_OP OP_CP KW_LESS KW_NIL KW_WHILE
%token KW_AND KW_APPEND KW_CONCAT KW_EQUAL KW_FALSE KW_LIST KW_NOT KW_OR KW_SET KW_TRUE 
%token KW_DEFFUN KW_FOR KW_IF KW_EXIT KW_LOAD KW_DISP STRING_VALUE OP_COMMA COMMENT
%left OP_PLUS OP_MINUS OP_MULT OP_DIV OP_DBLMULT
%token <value> VALUE
%token <identi> IDENTIFIER
%type <value> EXPI EXPB
%type <list> EXPLISTI VALUES LISTVALUE
%%
INPUT:  EXPI 
        {
            printf("%d",$1);
        } 
        |
        EXPB 
        {
            printf("%d",$1);
            
        }
        |
        EXPLISTI
        {
            if($1 == NULL){
                printf("NIL");
            }
            else{
                for(int i=1; i<=$1[0]; i++){
                    printf("%d ",$1[i]);
                }
            }
        };

LISTVALUE:  OP_OP KW_LIST VALUES OP_CP
            {
                $$=$3;
            };

VALUES: VALUES VALUE
        {
            $$=realloc($1,sizeof(int)*($1[0]+2));
            $$[0] = $1[0]+1;
            $$[$1[0]]=$2;
        }
        | 
        VALUE
        {
            $$=malloc(sizeof(int)*2);
            $$[0] = 1;
            $$[1] = $1;
        };

EXPI:   OP_OP OP_PLUS EXPI EXPI OP_CP
        {
            $$= $3 + $4;
        }
        |
        OP_OP OP_MINUS EXPI EXPI OP_CP
        {
            $$= $3 - $4;
        }
        |
        OP_OP OP_MULT EXPI EXPI OP_CP
        {
            $$= $3 * $4;
        }
        |
        OP_OP OP_DIV EXPI EXPI OP_CP
        {
            $$= $3 / $4;
        }
        |
        OP_OP OP_DBLMULT EXPI EXPI OP_CP
        {
            int result=1;
            for(int i=0; i<$4; i++){
                result=result*$4;
            }
            $$=result;
        }
        |
        IDENTIFIER
        {
            $$=$1.value;
        }
        |
        VALUE
        {
            $$=$1;
        }
        |
        OP_OP KW_SET IDENTIFIER EXPI OP_CP
        {
            $3.value=$4;
            $$=$3.value;
        }
        |
        OP_OP KW_IF EXPB EXPI OP_CP
        {
            if($3 == 1){
                $$ = $4;
            }
        }
        |
        OP_OP KW_IF EXPB EXPI EXPI OP_CP
        {
            if($3 == 1){
                $$ = $4;
            }
            else{
                $$ = $5;
            }
        }
        |
        OP_OP KW_WHILE OP_OP EXPB OP_CP EXPI OP_CP
        {
            while($4 == 1){
                $$ = $6;
            }
        }
        |
        OP_OP KW_FOR OP_OP IDENTIFIER EXPI EXPB OP_CP EXPI OP_CP
        {
            for($4.value = $5; $6 == 1; $4.value++){
                $$ = $8;
            }
        }
        |
        OP_OP KW_DISP EXPI OP_CP
        {
            printf("%d\n",$3);
            $$=1;
        }
        |
        OP_OP KW_DISP EXPLISTI OP_CP
        {
            for(int i=1; i<=$3[0]; i++){
                    printf("%d ",$3[i]);
            }
            printf("\n");
            $$=$3[0];
        }
        |
        OP_OP KW_DISP EXPB OP_CP
        {
            if($3 == 1){
                printf("true\n");
            }
            else{
                printf("false\n");
            }
            $$=$3;
        };

EXPB:   OP_OP KW_AND EXPB EXPB OP_CP 
        {
            if($3 && $4) $$=1; else $$=0;
        }
        |
        OP_OP KW_OR EXPB EXPB OP_CP
        {
            if($3 || $4) $$=1; else $$=0;
        }
        |
        OP_OP KW_NOT EXPB OP_CP
        {
            if($3) $$=0; else $$=1;
        }
        |
        OP_OP KW_EQUAL EXPB EXPB OP_CP
        {
            if($3 == $4) $$=1; else $$=0;
        }
        |
        OP_OP KW_EQUAL EXPI EXPI OP_CP
        {
            if($3 == $4) $$=1; else $$=0;
        }
        |
        OP_OP KW_LESS EXPB EXPB OP_CP
        {
            if($3 < $4) $$=1; else $$=0;
        }
        |
        OP_OP KW_LESS EXPI EXPI OP_CP
        {
            if($3 < $4) $$=1; else $$=0;
        }
        |
        KW_TRUE
        {
            $$=1;
        }
        |
        KW_FALSE
        {
            $$=0;
        };

EXPLISTI:   OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP
            {
                $$=malloc(sizeof(int)*($4[0]+$3[0]+1));
                for(int i=1; i<=$3[0]; i++){
                    $$[i]=$3[i];
                }
                for(int i=1; i<=$4[0]; i++){
                    $$[i+$3[0]]=$4[i];
                }
                $$[0]=$3[0]+$4[0];
                free($3);
                free($4);
            }
            |
            OP_OP KW_APPEND EXPI EXPLISTI OP_CP
            {
                $$=realloc($4,sizeof(int)*($4[0]+2));
                for(int i=$$[0]+1; i>1; i--){
                    $$[i]=$$[i-1];
                }
                $$[0]++;
                $$[1]=$3;
            }
            |
            LISTVALUE
            {
                $$ = $1;
            }
            |
            KW_NIL
            {
                $$=NULL;
            }
%%
int main()
{
    return(yyparse());
}
void yyerror(char *s){
    printf("Error");
    }