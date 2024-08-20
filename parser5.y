%{
    #include<stdio.h>
    #include<string.h>
    #include<stdlib.h>
    #include<ctype.h>
    #include"lex.yy.c"

    void yyerror(const char *s);
    int yylex();
    int yywrap();
    void add(char);
    void insert_type();
    int search(char *);
    void print_tree(struct node*);
    void print_inorder(struct node *);
    void check_declaration(char *);
    void check_return_type(char *);
    int check_types(char *, char *);
    char *get_type(char *);
    struct node* mknode(struct node *left, struct node *right, char *token);

    struct dataType {
        char * id_name;
        char * data_type;
        char * type;
        int line_no;
    } symbol_table[40];

    int count=0;
    int q;
    char type[10];
    extern int countn;
    struct node *head;
    int sem_errors=0;
    int ic_idx=0;
    int temp_var=0;
    int label=0;
    int is_for=0;
    char buff[100];
    char errors[10][100];
    char reserved[10][10] = {"int", "float", "char", "void", "if", "else", "for", "main", "return", "include"};
    char icg[50][100];

    struct node {
        struct node *left;
        struct node *right;
        char *token;
    };
%}

// Definição da união para os valores semânticos dos tokens.
%union { 
    struct var_name { 
        char name[100];
        struct node* nd;
    } nd_obj;

    struct var_name2 { 
        char name[100];
        struct node* nd;
        char type[5];
    } nd_obj2; 

    struct var_name3 {
        char name[100];
        struct node* nd;
        char if_body[5];
        char else_body[5];
    } nd_obj3;
} 

%token VOID 
%token PRINTFF SCANFF INT FLOAT CHAR VOID RETURN FOR IF ELSE INCLUDE TRUE FALSE NUMBER FLOAT_NUM ID UNARY LE GE EQ NE GT LT AND OR ADD SUBTRACT DIVIDE MULTIPLY STR CHARACTER

%type <nd_obj> headers main body return datatype statement arithmetic relop program else
%type <nd_obj2> init value expression
%type <nd_obj3> condition

%%

program: headers main '(' ')' '{' body return '}' { 
    $2.nd = mknode($6.nd, $7.nd, "main");
    $$.nd = mknode($1.nd, $2.nd, "program");
    head = $$.nd;
} 
;

headers: headers headers { 
    $$.nd = mknode($1.nd, $2.nd, "headers");
}
| INCLUDE { 
    add('H');
    $$.nd = mknode(NULL, NULL, $1.name);
}
;

main: datatype ID { 
    add('F');
}
;

datatype: INT { insert_type(); }
| FLOAT { insert_type(); }
| CHAR { insert_type(); }
| VOID { insert_type(); }
;

body: FOR { 
    add('K');
    is_for = 1;
} '(' statement ';' condition ';' statement ')' '{' body '}' { 
    struct node *temp = mknode($6.nd, $8.nd, "CONDITION");
    struct node *temp2 = mknode($4.nd, temp, "CONDITION");
    $$.nd = mknode(temp2, $11.nd, $1.name);
    sprintf(icg[ic_idx++], buff);
    sprintf(icg[ic_idx++], "JUMP to %s\n", $6.if_body);
    sprintf(icg[ic_idx++], "\nLABEL %s:\n", $6.else_body);
}
| IF { 
    add('K');
    is_for = 0;
} '(' condition ')' { 
    sprintf(icg[ic_idx++], "\nLABEL %s:\n", $4.if_body);
} '{' body '}' { 
    sprintf(icg[ic_idx++], "\nLABEL %s:\n", $4.else_body);
} else { 
    struct node *iff = mknode($4.nd, $8.nd, $1.name);
    $$.nd = mknode(iff, $11.nd, "if-else");
    sprintf(icg[ic_idx++], "GOTO next\n");
}
| statement ';' { 
    $$.nd = $1.nd;
}
| body body { 
    $$.nd = mknode($1.nd, $2.nd, "statements");
}
| PRINTFF { 
    add('K');
} '(' STR ')' ';' { 
    $$.nd = mknode(NULL, NULL, "printf");
}
| SCANFF { 
    add('K');
} '(' STR ',' '&' ID ')' ';' { 
    $$.nd = mknode(NULL, NULL, "scanf");
}
;

else: ELSE { 
    add('K');
} '{' body '}' { 
    $$.nd = $4.nd;
}
;

statement: datatype init { 
    $$.nd = mknode($1.nd, $2.nd, "declaration");
}
| init { 
    $$.nd = $1.nd;
}
| expression { 
    $$.nd = $1.nd;
}
| RETURN expression { 
    check_return_type($2.name);
    $$.nd = mknode(NULL, $2.nd, "return");
    sprintf(icg[ic_idx++], "return %s\n", $2.name);
}
;

init: ID { 
    check_declaration($1.name);
    $$.nd = mknode(NULL, NULL, $1.name);
}
| ID '=' value { 
    q = search($1.name);
    sprintf(icg[ic_idx++], "%s = %s\n", $1.name, $3.name);
    check_declaration($1.name);
    int c = check_types(get_type($1.name), type);
    if(c > 0) sem_errors++;
    $$.nd = mknode(NULL, $3.nd, "=");
}
;

value: expression { 
    $$.nd = $1.nd;
}
| NUMBER { 
    strcpy(type, "int");
    $$.nd = mknode(NULL, NULL, "NUMBER");
}
| FLOAT_NUM { 
    strcpy(type, "float");
    $$.nd = mknode(NULL, NULL, "FLOAT");
}
| ID { 
    check_declaration($1.name);
    strcpy(type, get_type($1.name));
    $$.nd = mknode(NULL, NULL, $1.name);
}
;

expression: expression ADD expression { 
    sprintf(icg[ic_idx++], "t%d = %s + %s\n", temp_var++, $1.name, $3.name);
    $$.nd = mknode($1.nd, $3.nd, "+");
}
| expression SUBTRACT expression { 
    sprintf(icg[ic_idx++], "t%d = %s - %s\n", temp_var++, $1.name, $3.name);
    $$.nd = mknode($1.nd, $3.nd, "-");
}
| expression MULTIPLY expression { 
    sprintf(icg[ic_idx++], "t%d = %s * %s\n", temp_var++, $1.name, $3.name);
    $$.nd = mknode($1.nd, $3.nd, "*");
}
| expression DIVIDE expression { 
    sprintf(icg[ic_idx++], "t%d = %s / %s\n", temp_var++, $1.name, $3.name);
    $$.nd = mknode($1.nd, $3.nd, "/");
}
| ID { 
    check_declaration($1.name);
    $$.nd = mknode(NULL, NULL, $1.name);
}
| NUMBER { 
    strcpy(type, "int");
    $$.nd = mknode(NULL, NULL, "NUMBER");
}
;

relop: expression LT expression { 
    sprintf(icg[ic_idx++], "t%d = %s < %s\n", temp_var++, $1.name, $3.name);
    $$.nd = mknode($1.nd, $3.nd, "<");
}
| expression GT expression { 
    sprintf(icg[ic_idx++], "t%d = %s > %s\n", temp_var++, $1.name, $3.name);
    $$.nd = mknode($1.nd, $3.nd, ">");
}
| expression LE expression { 
    sprintf(icg[ic_idx++], "t%d = %s <= %s\n", temp_var++, $1.name, $3.name);
    $$.nd = mknode($1.nd, $3.nd, "<=");
}
| expression GE expression { 
    sprintf(icg[ic_idx++], "t%d = %s >= %s\n", temp_var++, $1.name, $3.name);
    $$.nd = mknode($1.nd, $3.nd, ">=");
}
| expression EQ expression { 
    sprintf(icg[ic_idx++], "t%d = %s == %s\n", temp_var++, $1.name, $3.name);
    $$.nd = mknode($1.nd, $3.nd, "==");
}
| expression NE expression { 
    sprintf(icg[ic_idx++], "t%d = %s != %s\n", temp_var++, $1.name, $3.name);
    $$.nd = mknode($1.nd, $3.nd, "!=");
}
| TRUE { 
    $$.nd = mknode(NULL, NULL, "TRUE");
}
| FALSE { 
    $$.nd = mknode(NULL, NULL, "FALSE");
}
;

condition: ID { 
    check_declaration($1.name);
    $$.nd = mknode(NULL, NULL, $1.name);
}
| expression { 
    $$.nd = $1.nd;
}
| relop { 
    $$.nd = $1.nd;
}
| '(' condition ')' { 
    $$.nd = $2.nd;
}
;

return: RETURN { 
    $$.nd = mknode(NULL, NULL, "return");
}
;

%%
void yyerror(const char *s) {
    fprintf(stderr, "Error: %s\n", s);
}
