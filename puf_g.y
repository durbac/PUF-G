%{
/***************************************************************** 
 * Date of Creation: 01:00, Sunday, May 10, 2020
 * File Name: puf_g.y
 * Purpose: This is a PUF_G parser intended to convert from PUF_G 
            to C language and compute sample complexity for PAC
            learning the PUF construction specified
 * Author: Durba Chatterjee
 * Revision History:
******************************************************************/ 


/*****************************************************************
 * Header Include Section 
 *****************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/*****************************************************************
 * Macro Definition Section
 *****************************************************************/
#define defaultOutputFile "pac_analysis.txt"
#define INIT 0
#define NON_INIT 1
#define YYDEBUG 1 
#define PRINT 0

#if PRINT
  #define print(a) printf a
#else
  #define print(a) (void)0
#endif

/*****************************************************************
 * Global Structure Declaration Section 
 *****************************************************************/
typedef struct module_node {
    // char *modName;
    char *primitivePointer;
    struct statement_node *statementPointer;
    struct input_def_node *inputDefPointer;
    struct input_def_node *outputDefPointer;
    struct module_node* nextModule;
}ModuleNode;

typedef struct input_def_node {
    char *data_type;
    char *tuple;
    struct input_def_node *nextInputDef;
}InputDefNode;

typedef struct primitive_call_node {
    char *primitivePointer;
    char *inputVariable;
}PrimitiveCallNode;

typedef struct statement_node {
    struct assignment_node *assignmentPointer;
    struct if_else_statement_node *ifElsePointer;
    struct serial_statement_node *serialPointer;
    struct parallel_statement_node *parallelPointer;
    struct statement_node *nextStatement;
}StatementNode;

typedef struct assignment_node {
    char *assignmentName;
    struct expression_node* iteratorExpressionPointer;
    struct expression_node *expressionPointer;
    struct primitive_call_node *primitiveCallPointer;
}AssignmentNode;

typedef struct expression_node {
    struct expression_node *leftChild;
    struct expression_node *rightChild;
    char *op;
}ExpressionNode;

typedef struct if_else_statement_node{
    struct expression_node *ifExpressionPointer;
    struct statement_node *ifStatementPointer;
    struct elseif_statement_node *elseIfStatementPointer;
    struct statement_node *elseStatementPointer;
}IfElseStatementNode;

typedef struct elseif_statement_node {
    struct expression_node *ifExpressionPointer;
    struct statement_node *ifStatementPointer;
    struct elseif_statement_node *elseIfStatementPointer;
    struct statement_node *elseStatementPointer;
}ElseIfStatementNode;

typedef struct serial_statement_node {
    struct assignment_node *serialAssignmentPointer;
    struct expression_node *serialExpressionPointer;
    struct statement_node *serialStatementPointer;
}SerialStatementNode;

typedef struct parallel_statement_node {
    struct assignment_node *parallelAssignmentPointer;
    struct expression_node *parallelExpressionPointer;
    struct statement_node *parallelStatementPointer;
}ParallelStatementNode;

//end of puf primitves


//Node to store input parameters
typedef struct node {
    char *param;
    int coeff;
    int power;
    struct node *next;
}Node;

//Node to store list of submodules in a module
typedef struct primNode {
    char* primitiveName;
    ModuleNode* modulePointer;  //pointer to the module in the interim model
    char* inputVariable;        //input given to the module - each module instantiation might have different input param
    char* rep;                  //representation class chosen for the module
    char* ns;                   //noise sensitivity of module
    int visited;                //flag to check if the module instantiation has been visited during parsing
    struct primNode* nextPrim;
}PrimNode;

typedef struct adjacencyNode {
    char* varName;
    char* index; 
}adjacencyNode;

// graph representation of a module
typedef struct graphNode {
    char* varName;      //variable name
    char** adjList;     //list of variables connected to this node (directed graph) - out edges
    int outDeg;         // out degree
    int visited;        //to check if the variable has been traversed or not
    int isPrimitive;    // true if the variable is the output of a primitive call
    int inLoop;         // true if the variable is assigned within a parallel or serial block
    struct graphNode *nextGraphNode;
}GraphNode;

// to hold the start vertices - vertices with 0 in-degree 
typedef struct graphStartNode {
    GraphNode* initNode;
    struct graphStartNode* nextStartNode;
}GraphStartNode;

/*****************************************************************
 * Global Variable Declaration Section 
 *****************************************************************/
extern FILE *yyin, *yyout;
char *inputFile, *outputFile;
int state = INIT;

// int yydebug=1;

ModuleNode *headModuleNode;
Node **headnode = NULL; //for storing the parameters
GraphNode* headGraphNode = NULL;    // graph of a given module
GraphStartNode* headInitNode = NULL;

/*****************************************************************
 * Global Function Declaration Section 
 *****************************************************************/
extern int yylex();
int yyparse();
void yyerror(char *);

// to measure execution time
unsigned int timestamp(void) {
    unsigned int bottom, top;
    asm volatile("xorl %%eax, %%eax\n cpuid \n" ::: "%eax", "%ebx", "%ecx", "%edx"); 
    asm volatile("rdtsc\n" : "=a" (bottom), "=d" (top) ); 
    asm volatile("xorl %%eax, %%eax\n cpuid \n" ::: "%eax", "%ebx", "%ecx", "%edx");
    return bottom;
}

//Intermediate data-structure functions
ModuleNode *createModuleNode(char*, StatementNode*, InputDefNode*, InputDefNode*);
InputDefNode *createInputDefNode(char*, char*, InputDefNode*);
PrimitiveCallNode *createPrimitiveCallNode(char*, char*);
StatementNode *createStatementNode(AssignmentNode*, IfElseStatementNode*, SerialStatementNode*, ParallelStatementNode*);
AssignmentNode *createAssignmentNodeI(char*, ExpressionNode*, ExpressionNode*, PrimitiveCallNode*);
ExpressionNode *createExpressionNode(ExpressionNode*, ExpressionNode*, char*);
IfElseStatementNode *createIfElseStatementNode(ExpressionNode*, StatementNode*, ElseIfStatementNode*, StatementNode*);
ElseIfStatementNode *createElseIfStatementNode(ExpressionNode*, StatementNode*, ElseIfStatementNode*);//, StatementNode*);
SerialStatementNode *createSerialStatementNode(AssignmentNode*, ExpressionNode*, StatementNode*);
ParallelStatementNode *createParallelStatementNode(AssignmentNode*, ExpressionNode*, StatementNode*);



void extraxtPACComplexityFromPUFGObjectModel();

%}


%union{
    char *string;
    struct module_node *moduleNodePointer;
    struct input_def_node *inputDefNodePointer;
    struct primitive_call_node *primitiveCallNodePointer;
    struct statement_node *statementNodePointer;
    struct assignment_node *assignmentNodePointer;
    struct expression_node *expressionNodePointer;
    struct if_else_statement_node *ifElseStatementNodePointer;
    struct elseif_statement_node *elseIfStatementNodePointer;
    struct serial_statement_node *serialStatementNodePointer;
    struct parallel_statement_node *parallelStatementNodePointer;
}


%token <string> BEGIN_MOD
%token <string> END_MOD
%token <string> RETURN
%token <string> IF
%token <string> THEN
%token <string> ELSE
%token <string> ELSE_IF
%token <string> END_IF
%token <string> TO
%token <string> DO
%token <string> SERIAL
%token <string> END_SERIAL
%token <string> PARALLEL
%token <string> END_PARALLEL

%token <string> STRING
%token <string> NUMBER
%token <string> NUM
%token <string> VEC
%token <string> BIT


%token <string> APUF

%token <string> D_FLIPFLOP 
%token <string> MUX21
%token <string> DELAY_CHAIN
%token <string> ARBITER


%token <string> BOOL_AND // &
%token <string> BOOL_OR // |
%token <string> BOOL_NOT // !
%token <string> BOOL_XOR // ^


%token <string> EQUALITY // ==
%token <string> INEQUALITY // !=
%token <string> LESSER_THAN_EQUAL // <=
%token <string> GREATER_THAN_EQUAL // >=
%token <string> LESSER_THAN // <
%token <string> GREATER_THAN // >
%token <string> ADD // +
%token <string> SUBTRACT // -
%token <string> MULTIPLY // *
%token <string> DIVIDE // /
%token <string> POWER // **
%token <string> MODULO // %
%token <string> ASSIGN // =

%token <string> OPEN_PARENTHESIS // (
%token <string> CLOSE_PARENTHESIS // )
%token <string> OPEN_BRACKET // {
%token <string> CLOSE_BRACKET // }
%token <string> OPEN_BRACE // [
%token <string> CLOSE_BRACE // ]
%token <string> COMMA // ,
%token <string> SEMI_COLON // ;
%token <string> COLON // :

//arrange tokens in order of precedence
%nonassoc OPEN_PARENTHESIS CLOSE_PARENTHESIS
%nonassoc OPEN_BRACE CLOSE_BRACE
%left BOOL_AND BOOL_OR BOOL_XOR 
%nonassoc BOOL_NOT
// %nonassoc ASSIGN
%nonassoc LESS_THAN_ASSIGN
%nonassoc EQUALITY INEQUALITY ASSIGN
%left ADD SUBTRACT MULTIPLY DIVIDE 
%right MODULO GREATER_THAN GREATER_THAN_EQUAL LESSER_THAN LESSER_THAN_EQUAL POWER
// %right BLOCK_ASSIGNMENT BLOCK_ASSIGNMENT_DELIM
%nonassoc ELSE_IF_PREC
%nonassoc ELSE
%nonassoc END_IF
// %nonassoc BLOCK_ASSIGNMENT


%type <moduleNodePointer> top
%type <moduleNodePointer> module
%type <inputDefNodePointer> input_def
%type <inputDefNodePointer> output_def
%type <primitiveCallNodePointer> primitive_call
%type <statementNodePointer> statements
%type <statementNodePointer> statement
%type <assignmentNodePointer> assignment
%type <expressionNodePointer> expression
%type <ifElseStatementNodePointer> if_else_statement
%type <elseIfStatementNodePointer> elseif_statement
%type <serialStatementNodePointer> serial_statement
%type <parallelStatementNodePointer> parallel_statement

%type <string> tuple
%type <string> variable
%type <string> delimeter
%type <string> data_type

%start top

%%


top:
    module top
    {
        print(("------in top module rule 1\n"));
        $1->nextModule = $2;
        $$ = $1; 
    }
  | module 
    {
        print(("------in top module rule 2\n"));
        $$ = $1;
    } 
    ;

module:
        BEGIN_MOD STRING OPEN_PARENTHESIS input_def CLOSE_PARENTHESIS
            statements output_def
        END_MOD STRING
        {   // print(("in module rule\n"));
            $$ = createModuleNode($2, $6, $4, $7);
            if(state == INIT) {
                // print(("in init state\n"));
                headModuleNode = $$;
                state = NON_INIT;
            }
        }
        ;

input_def:  data_type tuple delimeter input_def 
            {
                $$ = createInputDefNode($1, $2, $4);//InputDefNode *t
                // t->nextInputDef = $4;
                // $$ = t;
            }
          | data_type tuple
            {
                $$ = createInputDefNode($1, $2, NULL);
            }
          | //no-input
            {
                $$ = NULL;
            }
            ;

output_def: RETURN OPEN_PARENTHESIS input_def CLOSE_PARENTHESIS SEMI_COLON
            {
                $$ = $3;
            }
          | //no-output
            {
                $$ = NULL;
            }
            ;

primitive_call: STRING OPEN_PARENTHESIS variable CLOSE_PARENTHESIS
                {   // print(("in primitive_call rule\n"));
                    $$ = createPrimitiveCallNode($1, $3);
                }
                ;

tuple:  LESSER_THAN variable GREATER_THAN//check in grammar
        {
            $$ = $2;
        }
      | STRING %prec LESS_THAN_ASSIGN
        {
            $$ = $1;
        }
      | NUMBER
        {
            $$ = $1;
        }
        ;

variable:   tuple delimeter variable
            {   print(("in variable rule %s %s %s \n", $1, $2, $3));
                $$ = strcat(strcat($1, $2),$3);
            }
          | tuple
            {
                $$ = $1;
            }
          | //null
            {
                $$ = "";
            }
            ;

delimeter:  SEMI_COLON
            {
                $$ = $1;
            }
          | COMMA{
                $$ = $1;
            }
            ;

data_type:  NUM
            {
                $$ = $1;
            }
          | VEC
            {
                $$ = $1;
            }
          | BIT 
            {
                $$ = $1;
            }
            ;

statements:  
            statement statements
            {
                $1->nextStatement = $2;
                $$ = $1;
            }
          | statement
            {
                $$ = $1;
            }
            ;

statement:  
            assignment
            {
                $$ = createStatementNode($1, NULL, NULL, NULL);
            }
          | if_else_statement
            {
                $$ = createStatementNode(NULL, $1, NULL, NULL);
            }
          | serial_statement
            {
                $$ = createStatementNode(NULL, NULL, $1, NULL);
            }
          | parallel_statement
            {
                $$ = createStatementNode(NULL, NULL, NULL, $1);
            }
            ;

assignment:
            STRING ASSIGN expression delimeter //%prec BLOCK_ASSIGNMENT_DELIM
            {
                $$ = createAssignmentNodeI($1, NULL, $3, NULL);
            }
          | STRING ASSIGN expression //%prec BLOCK_ASSIGNMENT//added later for iterator assignment in serial and parallel blocks
            {   print(("in assignment rule: string assign expression\n"));
                $$ = createAssignmentNodeI($1, NULL, $3, NULL);
            }
          | tuple ASSIGN primitive_call delimeter
            {   print(("in assignment rule: tuple assign primitive_call delim\n"));
                $$ = createAssignmentNodeI($1, NULL, NULL, $3);
            }
          | STRING ASSIGN primitive_call delimeter  //added later
            {   print(("in assignment rule: string assign primitive_call delim %s \n", $3->primitivePointer));
                $$ = createAssignmentNodeI($1, NULL, NULL, $3);
            }
            //added later to allow using indexes in vectors
          | STRING OPEN_BRACE expression CLOSE_BRACE ASSIGN expression delimeter 
            {   print(("in assignment rule: STRING OPEN_BRACE expression CLOSE_BRACE ASSIGN expression delimeter \n"));
                $$ = createAssignmentNodeI($1, $3, $6, NULL);
            }
          // | STRING OPEN_BRACE expression CLOSE_BRACE ASSIGN expression //added later for iterator assignment in serial and parallel blocks
          //   {   print(("in assignment rule: STRING OPEN_BRACE expression CLOSE_BRACE ASSIGN expression\n");
          //       $$ = createAssignmentNodeI($1, $3, $6, NULL);
          //   }
          | STRING OPEN_BRACE expression CLOSE_BRACE ASSIGN primitive_call delimeter  //added later
            {   print(("in assignment rule: STRING OPEN_BRACE expression CLOSE_BRACE ASSIGN primitive_call delimeter\n"));
                $$ = createAssignmentNodeI($1, $3, NULL, $6);
            }
          | delimeter//null-statement
            {
                $$ = NULL;
            }
            ;

expression:
            expression ADD expression
            {
                $$ = createExpressionNode($1, $3, $2); 
            }
          | expression SUBTRACT expression
            {
                $$ = createExpressionNode($1, $3, $2); 
            }
          | expression MULTIPLY expression
            {
                $$ = createExpressionNode($1, $3, $2); 
            }
          | expression DIVIDE expression
            {
                $$ = createExpressionNode($1, $3, $2); 
            }
          | expression POWER expression
            {
                $$ = createExpressionNode($1, $3, $2); 
            }
          | expression MODULO expression
            {   // print(("========================>   Modulo Operation used\n"));
                $$ = createExpressionNode($1, $3, $2); 
            }
          | expression BOOL_AND expression
            {
                $$ = createExpressionNode($1, $3, $2); 
            }
          | expression BOOL_OR expression
            {
                $$ = createExpressionNode($1, $3, $2); 
            }
          | expression BOOL_XOR expression
            {   print(("expression BOOL_XOR expression\n"));
                $$ = createExpressionNode($1, $3, $2); 
            }
          | expression EQUALITY expression
            {
                $$ = createExpressionNode($1, $3, $2); 
            }
          | expression INEQUALITY expression
            {
                $$ = createExpressionNode($1, $3, $2); 
            }
          | expression GREATER_THAN_EQUAL expression
            {
                $$ = createExpressionNode($1, $3, $2); 
            }
          | expression LESSER_THAN_EQUAL expression
            {
                $$ = createExpressionNode($1, $3, $2); 
            }
          | expression GREATER_THAN expression
            {
                $$ = createExpressionNode($1, $3, $2); 
            }
          | expression LESSER_THAN expression
            {
                $$ = createExpressionNode($1, $3, $2); 
            }
          | OPEN_PARENTHESIS expression CLOSE_PARENTHESIS
            {
                $$ = createExpressionNode($2, NULL, $1);
            }
          | expression OPEN_BRACE expression CLOSE_BRACE //%prec OPEN_BRACE
            {   print(("expression rule : expression OPEN_BRACE expression CLOSE_BRACE\n"));
                $$ = createExpressionNode($1, $3, $2);
            }
          | BOOL_NOT expression
            {
                $$ = createExpressionNode($2, NULL, $1);
            }
          | STRING
            {   // print(("=========================> String used\n"));
                $$ = createExpressionNode(NULL, NULL, $1);
            }
          | NUMBER
            {
                $$ = createExpressionNode(NULL, NULL, $1);
            }
            ;

if_else_statement:  IF expression THEN statements END_IF //%prec END_IF
                    {
                        $$ = createIfElseStatementNode($2, $4, NULL, NULL);
                    }
                  | IF expression THEN
                        statements
                    END_IF
                    ELSE
                        statements
                    END_IF //%prec ELSE
                    {
                        $$ = createIfElseStatementNode($2, $4, NULL, $7);
                    }
                  | IF expression THEN
                        statements
                    END_IF
                    elseif_statement 
                    ELSE 
                        statements
                    END_IF %prec ELSE_IF_PREC
                    {
                        $$ = createIfElseStatementNode($2, $4, $6, NULL);
                    }
                    ;

elseif_statement:   ELSE IF expression THEN statements END_IF
                    elseif_statement
                    {
                        $$ = createElseIfStatementNode($3, $5, $7);//, $9);
                    }
                  | ELSE IF expression THEN
                        statements
                    END_IF //%prec LOWER_THAN_IF
                    {
                        $$ = createElseIfStatementNode($3, $5, NULL);//, $8);
                    }
                    ;

serial_statement:   SERIAL assignment TO expression DO statements END_SERIAL
                    {
                        $$ = createSerialStatementNode($2, $4, $6);
                    }
                    ;

parallel_statement: PARALLEL assignment TO expression DO statements END_PARALLEL
                    {
                        $$ = createParallelStatementNode($2, $4, $6);
                    }
                    ;

%%

/*****************************************************************
 * Function Definition Section 
 *****************************************************************/

void yyerror(char *str) {
    printf("Error(From yyerror): %s\n",str);
    // print(("line %d: %s\n", yylineno, str)); 
    return;
}

ModuleNode *createModuleNode(char *primitive, StatementNode *statement, InputDefNode *input_def, InputDefNode *output_def) {
    print(("createModuleNode\n"));
    ModuleNode *modNode;
    if((modNode = (ModuleNode*)malloc(sizeof(ModuleNode)))!=NULL) {
        // modNode->modName = name;
        modNode->primitivePointer = primitive;
        modNode->statementPointer = statement;
        modNode->inputDefPointer = input_def;
        modNode->outputDefPointer = output_def;
        modNode->nextModule = NULL;
    }
    else {
        printf("Malloc Error: Cannot Create Module node!\n");
    }
    return (modNode);
}

PrimitiveCallNode *createPrimitiveCallNode(char *primitive, char *input) {
    // printf("createPrimitiveCallNode\n");
    PrimitiveCallNode *primcall;
    if((primcall = (PrimitiveCallNode*)malloc(sizeof(PrimitiveCallNode)))!=NULL) {
        primcall-> primitivePointer = primitive;
        primcall-> inputVariable = input;
    }
    else {
        printf("Malloc Error: Cannot Create Primitive Call node!\n");
    }
    return (primcall);
}

InputDefNode *createInputDefNode(char *datatype, char *tuple, InputDefNode *next) {
    // printf("createInputDefNode\n");
    InputDefNode *inputdef;
    if((inputdef = (InputDefNode*)malloc(sizeof(InputDefNode))) != NULL) {
        inputdef-> data_type = datatype;
        inputdef-> tuple = tuple;
        inputdef-> nextInputDef = next;
    }
    else {
        printf("Malloc Error: Cannot Create Input Def Node!\n");
    }
    return (inputdef);
}

StatementNode *createStatementNode(AssignmentNode *assignmentnode, IfElseStatementNode *ifelseStatement, SerialStatementNode *serialnode, ParallelStatementNode *parallelnode) {
    // printf("createStatementNode\n");
    StatementNode *stateNode;
    if((stateNode = (StatementNode*)malloc(sizeof(StatementNode))) != NULL) {
        stateNode-> assignmentPointer = assignmentnode;
        stateNode-> ifElsePointer = ifelseStatement;
        stateNode-> serialPointer = serialnode;
        stateNode-> parallelPointer = parallelnode;
        stateNode-> nextStatement = NULL;
    }
    else {
        printf("Malloc Error: Cannot Create Statement node!\n");
    }
    return (stateNode);
}

/*AssignmentNode *createAssignmentNode(char *assignname, ExpressionNode *expression, PrimitiveCallNode *primitivecall) {
    // printf("createAssignmentNode\n");
    AssignmentNode *assignNode;
    if((assignNode = (AssignmentNode*)malloc(sizeof(AssignmentNode))) != NULL) {
        assignNode-> assignmentName = assignname;
        assignNode-> expressionPointer = expression;
        assignNode-> primitiveCallPointer = primitivecall;
    }
    else {
        printf("Malloc Error: Cannot Create Assignment node!\n");
    }
    return (assignNode);
}*/

AssignmentNode *createAssignmentNodeI(char *assignname, ExpressionNode *iterexpression, ExpressionNode *expression, PrimitiveCallNode *primitivecall) {
    print(("createAssignmentNodeI %d\n", (iterexpression!=NULL)));
    AssignmentNode *assignNode;
    if((assignNode = (AssignmentNode*)malloc(sizeof(AssignmentNode))) != NULL) {
        assignNode-> assignmentName = assignname;
        assignNode-> iteratorExpressionPointer = iterexpression;
        assignNode-> expressionPointer = expression;
        assignNode-> primitiveCallPointer = primitivecall;
    }
    else {
        printf("Malloc Error: Cannot Create Assignment node!\n");
    }
    return (assignNode);
}

ExpressionNode *createExpressionNode(ExpressionNode *left, ExpressionNode *right, char *operator) {
    // printf("createExpressionNode\n");
    ExpressionNode *expressionNode;
    if((expressionNode = (ExpressionNode*)malloc(sizeof(ExpressionNode))) != NULL) {
        expressionNode-> leftChild = left;
        expressionNode-> rightChild = right;
        expressionNode-> op = operator;
    }
    else {
        printf("Malloc Error: Cannot Create Expression node!\n");
    }
    return (expressionNode);
}

IfElseStatementNode *createIfElseStatementNode(ExpressionNode *ifexpression, StatementNode *ifstatement, ElseIfStatementNode *elseifblock, StatementNode *elsestatement) {
    // printf("createIfElseStatementNode\n");
    IfElseStatementNode *ifelsenode;
    if((ifelsenode = (IfElseStatementNode*)malloc(sizeof(IfElseStatementNode))) != NULL) {
        ifelsenode-> ifExpressionPointer = ifexpression;
        ifelsenode-> ifStatementPointer = ifstatement;
        ifelsenode-> elseIfStatementPointer = elseifblock;
        ifelsenode-> elseStatementPointer = elsestatement;
    }
    else {
        printf("Malloc Error: Cannot Create If Else Statement node!\n");
    }
    return (ifelsenode);
}

ElseIfStatementNode *createElseIfStatementNode(ExpressionNode *ifexpression, StatementNode *ifstatement, ElseIfStatementNode *elseifnext) {//, StatementNode *elsestatement) {
    // printf("createElseIfStatementNode\n");
    ElseIfStatementNode *elseifnode;
    if((elseifnode = (ElseIfStatementNode*)malloc(sizeof(ElseIfStatementNode))) != NULL) {
        elseifnode-> ifExpressionPointer = ifexpression;
        elseifnode-> ifStatementPointer = ifstatement;
        elseifnode-> elseIfStatementPointer = elseifnext;
        // elseifnode-> elseStatementPointer = elsestatement;
    }
    else {  
        printf("Malloc Error: Cannot Create Else If Statement node!\n");
    }
    return (elseifnode);
}

SerialStatementNode *createSerialStatementNode(AssignmentNode *serialassign, ExpressionNode *serialexpression, StatementNode *serialstatement) {
    // printf("createSerialStatementNode\n");
    SerialStatementNode *serialblock;
    if((serialblock = (SerialStatementNode*)malloc(sizeof(SerialStatementNode))) != NULL) {
        serialblock-> serialAssignmentPointer = serialassign;
        serialblock-> serialExpressionPointer = serialexpression;
        serialblock-> serialStatementPointer = serialstatement;
    }
    else {
        printf("Malloc Error: Cannot Create Serial Statement node!\n");
    }
    return (serialblock);
}

ParallelStatementNode *createParallelStatementNode(AssignmentNode *parallelassign, ExpressionNode *parallelexpression, StatementNode *parallelstatement) {
    // printf("createParallelStatementNode\n");
    ParallelStatementNode *parallelblock;
    if((parallelblock = (ParallelStatementNode*)malloc(sizeof(ParallelStatementNode))) != NULL) {
        parallelblock-> parallelAssignmentPointer = parallelassign;
        parallelblock-> parallelExpressionPointer = parallelexpression;
        parallelblock-> parallelStatementPointer = parallelstatement;
    }
    else {
        printf("Malloc Error: Cannot Create Parallel Statement node!\n");
    }
    return (parallelblock);
}

/*
*   For PAC analysis: Model and algorithm selection
*/
//function checked

void printParam(Node* head) {
    Node* temp = head;
    while(temp) {
        printf("temp->param = %s \t temp->coeff = %d \t temp->power = %d\n", temp->param, temp->coeff, temp->power);
        temp = temp->next;
    }
}

Node* handleInput(InputDefNode *input) {
    // printf("handleInput %d\n", input!=NULL);
    Node *newnode, *temp = NULL;
    Node* head = NULL;
    while(input) {
        if(strcmp(input->data_type, "num") == 0) {
            // printf("%s %s\n", input->data_type, input->tuple);
            if((newnode = (Node*)malloc(sizeof(Node))) != NULL) {
                newnode->param = input->tuple;
                newnode->coeff = 1;
                newnode->power = 1;
            }
            else {
                printf("Malloc Error: Cannot Create param node!\n");
            }
            if(head==NULL) {
                head = newnode;
                temp = head;
            }
            else {
                temp->next = newnode;
                temp = temp->next;
            }
        }
        input = input->nextInputDef;    
    }
    
    /*temp = head;
    while(temp) {
        printf("temp->param = %s \t temp->coeff = %d \t temp->power = %d\n", temp->param, temp->coeff, temp->power);
        temp = temp->next;
    }*/
    return head;
}

char* getPUFChallenge(InputDefNode *input) {
    // print(("getPUFChallenge %d\n", input!=NULL));
    while(input) {
        if(strcmp(input->data_type, "vec") == 0) {
            return input->tuple;
        }
        input = input->nextInputDef;
    }
    printf("Challenge not specified in Input Definition!!!\n");
    return NULL;
}

void printExpressionTree(ExpressionNode *exp) {
    printf("------------------in printExpressionTree %d\n", exp!=NULL);
    if(!exp)
        printf("NULL");
    if(!exp->leftChild && !exp->rightChild) {
        printf("none op = %s\n", exp->op);
        return;
    }
    else if(!exp->rightChild) {
        printf("no rightChild\n");
        if(strcmp(exp->op, "!")==0) { //*(exp->op) == BOOL_NOT
            // printf("no rigtt operator\n");
            printExpressionTree(exp);
            // printf("%s",strcat(strcat("(!", t), ")"));//NOT(exp);
            return;
        }
        //updated ")" to "(" on 21 aug 2021
        else if(strcmp(exp->op, "(")==0) { //*(exp->op) == OPEN_PARENTHESIS
            // printf("right parenthesis\n");
            printExpressionTree(exp);
            // printf("%s", strcat(strcat("(", t), ")"));//(exp);
            return;
        }
    }
    else if(exp->leftChild && exp->rightChild) {
        printf("in both else op = %s\n", exp->op);
        char* exp_all;    
        char *opd1;
        char *opd2;
        if(strcmp(exp->op, "[")==0) {
            printf("before leftchild %s\n",exp->op);
            printExpressionTree(exp->leftChild);
            printf("before rightchild %s\n", exp->op);
            printExpressionTree(exp->rightChild);
            // printf("%s \t %s \t %s\n", exp->op, opd1, opd2);
            printf("index expression\n");
            // exp_all = strcat(strcat(strcat(opd1,"["),opd2),"]");
            return;
        }
        else {
            printf("before leftchild %s\n",exp->op);
            printExpressionTree(exp->leftChild);
            printf("before rightchild %s\n", exp->op);
            printExpressionTree(exp->rightChild);
            // printf("%s \t %s \t %s\n", exp->op, opd1, opd2);
            // exp_all = strcat(strcat(opd1,exp->op),opd2);
        }
        // printf("%s \t %s \t %s \t exp_all = %s\n", exp->op, opd1, opd2, exp_all);
        // printf("exp_all = %s\n", exp_all);
        return;
    }
}

char* evaluateExpression(ExpressionNode *exp) {
    // printf("evaluateExpression %s %d %d\n", exp->op, exp->leftChild!=NULL, exp->rightChild!=NULL);
    // if(exp->leftChild)
    //     printf("op = %s lc = %s\n", exp->op, exp->leftChild->op);
    // if(exp->rightChild)
    //     printf("op = %s rc = %s\n", exp->op, exp->rightChild->op);
    if(!exp)
        return NULL;
    if(!exp->leftChild && !exp->rightChild) {
        // printf("leaf node %s\n", exp->op);
        return exp->op;
    }
    else if(!exp->rightChild) {
        // printf("no rigtt operator\n");
        if(strcmp(exp->op, "!")==0) { //*(exp->op) == BOOL_NOT
            char* t = evaluateExpression(exp);
            return strcat(strcat("(!", t), ")");//NOT(exp);
        }
        //updated ")" to "(" on 21 aug 2021
        else if(strcmp(exp->op, "(")==0) { //*(exp->op) == OPEN_PARENTHESIS
            // printf("right parenthesis\n");
            char* t = evaluateExpression(exp);
            return strcat(strcat("(", t), ")");//(exp);
        }
    }
    else if(exp->leftChild && exp->rightChild) {
        // printf("both child\n");
        char* exp_all;    
        char *opd1 = evaluateExpression(exp->leftChild);
        char *opd2 = evaluateExpression(exp->rightChild);
        if(strcmp(exp->op, "[")==0) {
            // printf("before leftchild %s %s\n",exp->op, exp->leftChild->op);
            // opd1 = evaluateExpression(exp->leftChild);
            // printf("before rightchild %s %s\n", exp->op, exp->rightChild->op);
            // opd2 = evaluateExpression(exp->rightChild);
            // printf("%s \t %s \t %s\n", exp->op, opd1, opd2);
            exp_all = strcat(strcat(strcat(opd1,"["),opd2),"]");
            return exp_all;
        }
        else {
            // printf("before leftchild %s %s\n",exp->op, exp->leftChild->op);
            // opd1 = evaluateExpression(exp->leftChild);
            // printf("before rightchild %s %s\n", exp->op, exp->rightChild->op);
            // opd2 = evaluateExpression(exp->rightChild);
            // printf("%s \t %s \t %s\n", exp->op, opd1, opd2);
            // char* exp_all = strcat(strcat(strcat(strcat("(",opd1),exp->op),opd2),")");
            exp_all = strcat(strcat(opd1,exp->op),opd2);
        }
        // printf("%s \t %s \t %s \t exp_all = %s\n\n", exp->op, opd1, opd2, exp_all);
        return exp_all;
    }
}

/*modified - updating on copy variables.. but since these point to actual variables, on doing strcpy, copy variables ar also getting updated.. a[i]XORa[i] problem not solved
char* evaluateExpression(ExpressionNode *exp) {
    printf("evaluateExpression %s %d %d\n", exp->op, exp->leftChild!=NULL, exp->rightChild!=NULL);
    if(!exp)
        return NULL;
    if(!exp->leftChild && !exp->rightChild) {
        // printf("leaf node %s\n", exp->op);
        return exp->op;
    }
    else if(!exp->rightChild) {
        // printf("no rigtt operator\n");
        if(strcmp(exp->op, "!")==0) { //*(exp->op) == BOOL_NOT
            char* t = evaluateExpression(exp);
            char* t_copy = (char*)malloc(sizeof(char)*strlen(t));
            strcpy(t_copy,t);
            return strcat(strcat("(!", t_copy), ")");//NOT(exp);
        }
        //updated ")" to "(" on 21 aug 2021
        else if(strcmp(exp->op, "(")==0) { //*(exp->op) == OPEN_PARENTHESIS
            // printf("right parenthesis\n");
            char* t = evaluateExpression(exp);
            char* t_copy = (char*)malloc(sizeof(char)*strlen(t));
            strcpy(t_copy,t);
            return strcat(strcat("(", t_copy), ")");//(exp);
        }
    }
    else if(exp->leftChild && exp->rightChild) {
        // printf("both child\n");
        char* exp_all;    
        char *opd1 = evaluateExpression(exp->leftChild);
        char *opd2 = evaluateExpression(exp->rightChild);
        char* opd1_copy = (char*)malloc(sizeof(char)*strlen(opd1));
        char* opd2_copy = (char*)malloc(sizeof(char)*strlen(opd2));
        strcpy(opd1_copy,opd1);
        strcpy(opd2_copy,opd2);
        if(strcmp(exp->op, "[")==0) {
            printf("op = %s \t opd1 = %s \t opd2 = %s\n", exp->op, opd1, opd2);
            exp_all = strcat(strcat(strcat(opd1_copy,"["),opd2_copy),"]");
            printf("exp_all = %s \t\t opd1_copy = %s \t\t opd2_copy = %s\n", exp_all, opd1_copy, opd2_copy);
            return exp_all;
        }
        else {
            printf("op = %s \t opd1 = %s \t opd2 = %s\n", exp->op, opd1, opd2);
            exp_all = strcat(strcat(opd1,exp->op),opd2);
            printf("exp_all = %s \t\t opd1_copy = %s \t\t opd2_copy = %s\n", exp_all, opd1_copy, opd2_copy);
        }
        printf("op = %s \t opd1 = %s \t opd2 = %s \t exp_all = %s\n\n", exp->op, opd1, opd2, exp_all);
        return exp_all;
    }
}*/

char* getModuleName(ModuleNode *module) {
    // printf("getModuleName\n");
    if(module)
        return module->primitivePointer;
    // else {
    //     printf("Module does not exist!!!\n");
    //     break;
    // }
}

int isPrimitive(ModuleNode *module) {
    // printf("isPrimitive %s\n", module->primitivePointer);
    if(module) {
        if(strcmp(module->primitivePointer,"APUF")==0 || strcmp(module->primitivePointer,"ROPUF")==0 || strcmp(module->primitivePointer,"DELAY_CHAIN")==0 
            || strcmp(module->primitivePointer,"ARBITER")==0 || strcmp(module->primitivePointer,"SWITCH_2X2")==0 || strcmp(module->primitivePointer, "MUX_2X1")==0)
            return 1;
    }
    return 0;
}

// map primitives to representations - primitive names are considered to be fixed
char* getRepresentation(char *type) {
    printf("\ngetRepresentation %s\n", type);
    char *rep;
    if(strcmp(type,"DELAY_CHAIN")==0) 
        rep = "LTF";
    else if(strcmp(type,"APUF")==0)
        rep = "DFA";
    else if(strcmp(type,"ROPUF")==0)
        rep = "DL";
    else if(strcmp(type,"ARBITER")==0)   //to be checked at parent module 
        rep = "-";
    else if(strcmp(type,"SWITCH_2X2")==0)   //to be checked at parent module 
        rep = "-";
    else if(strcmp(type,"MUX_2X1")==0)   //to be checked at parent module 
        rep = "-";
    return rep;
}

// modify the use of this function call to incorporate the power returned instead of NS
char* getNoiseSensitivity(char* rep) {
    // printf("getNoiseSensitivity %s\n", rep);
    char *ns_power;
    if(strcmp(rep,"LTF")==0) 
        ns_power = "1/2";   // ns = \epsilon^{1/2}
    else if(strcmp(rep,"DL")==0) 
        ns_power = "1/2";   // ns = \epsilon^{1/2}
    return ns_power;
}

//change representation depending on the input and output representations - this function assumes only one type of submodule in a given module
char* composeRepresentation(char* rep, char* it, char* ot) {
    printf("\ncomposeRepresentation rep = %s \t it = %s \t ot = %s\n", rep, it, ot);
    char* composed_rep;
    if(it==NULL && ot==NULL) 
        return rep;
    else if(ot==NULL) {//for only input transformations
        if(strcmp(rep,"LTF")==0) {
            composed_rep = "LTF_N";
        }
        else if(strcmp(rep, "DFA")==0) {    //for apuf
            composed_rep = "LTF_N";
        }
        else if(strcmp(rep, "-")==0) {
            composed_rep = "LTF_N";
        }
    }
    else if(it==NULL) {// for only output transformations
        if(strcmp(rep , "LTF")==0 || strcmp(rep , "LTF_N")==0) {
            composed_rep = rep;
        }
        else if(strcmp(rep , "DFA")==0) {  //dc: change this, transform dfa to ltf depending on the output transformation
            composed_rep = "LTF";
        }
        else if(strcmp(rep , "DL")==0) {  //dc: change composed representation class to DL if constituent PUFs can be represented by DL
            composed_rep = "DL";
        }
    }
    return composed_rep;
}


//change function to incorporate new results in other PAC Settings
/*
    This function returns the representation of composition of two PUF components. Changes representation depending on the input and output representations.
    This needs to be updated constantly as per new composition rules.
*/
char* composeRepresentationGeneric(char* rep1, char* rep2, char* ot) {
    printf("\ncomposeRepresentationGeneric rep1 = %s \t rep2 = %s \t ot = %s\n", rep1, rep2, ot);
    char* composed_rep = "-";
    // add case for other output operations
    if(strcmp(ot, "xor")==0) {
        if(strcmp(rep1, "DFA")==0  && strcmp(rep2, "DFA")==0) // XOR composition of DFA is DFA
            composed_rep = "DFA";
        else if((strcmp(rep1, "DFA")==0 || strcmp(rep1, "LTF")==0 || strcmp(rep1, "LTF_N")==0) && (strcmp(rep2, "DFA")==0 || strcmp(rep2, "LTF")==0 || strcmp(rep2, "LTF_N")==0))  // XOR composition of DFA and LTF is LTF
            composed_rep = "LTF_N";
        else if(strcmp(rep1, "LTF_N")!=0 && strcmp(rep2, "LTF_N")!=0) //XOR composition of LTFs is LTF (without noise)
            composed_rep = "LTF";
        else if(strcmp(rep1, "DL")!=0 && strcmp(rep2, "DL")!=0)  //XOR composition of DLs is DL
            composed_rep = "DL";
    }
    else {  //other output combiner functions
        if(strcmp(rep1, "DL")!=0 && strcmp(rep2, "DL")!=0) //any output composition of DL results in a DL
            composed_rep = "DL";
        if(strcmp(rep1, "DFA")!=0 && strcmp(rep2, "DFA")!=0) //any output composition of DL results in a DL
            composed_rep = "DFA";
        // cannot say the same for LTFs
    }
    //input transformation operations - interpose - DFA is used for APUF
    if(strcmp(ot, "interpose")==0) {
        if((strcmp(rep1, "DFA")==0 || strcmp(rep1, "LTF")==0 || strcmp(rep1, "LTF_N")==0) && (strcmp(rep2, "DFA")==0 || strcmp(rep2, "LTF")==0 || strcmp(rep2, "LTF_N")==0)) 
            composed_rep = "LTF_N";
    }
    //input operations - feed-forward - DFA is used for APUF
    else if(strcmp(ot, "feed-forward")==0) {
        if((strcmp(rep1, "DFA")==0 || strcmp(rep1, "LTF")==0 || strcmp(rep1, "LTF_N")==0)) 
            composed_rep = "LTF_N";
    }
    //input operations - recurrence - DFA is used for APUF
    else if(strcmp(ot, "recurrence")==0) {
        if((strcmp(rep1, "DFA")==0 || strcmp(rep1, "LTF")==0 || strcmp(rep1, "LTF_N")==0)) 
            composed_rep = "LTF_N";
    }
    printf("composed_rep =%s \n", composed_rep);
    return composed_rep;
}

// change function to incorporate modification of ns and not rep
/*
    This function returns the composition of noise sensitivity depending on the input and output transformation operations. 
    This needs to be updated as per new operations and composition rules. 
*/
char* composeNoiseSensitivity(char* ns, char* rep, char* it, char* ot, Node* param) {
    printf("\ncomposeNoiseSensitivity ns = %s \t rep = %s \t it = %s \t ot = %s\n", ns, rep, it, ot);
    printParam(param);
    char* composed_ns;
    if(it==NULL && ot==NULL) 
        return ns;
    else if(ot==NULL) {//for only input transformations
        if(strcmp(rep,"LTF")==0) {
            composed_ns = ns;
        }
        else if(strcmp(rep, "DFA")==0) {    //for apuf
            composed_ns = ns;
        }
        else if(strcmp(rep, "-")==0) {
            composed_ns = ns;
        }
    }
    else if(it==NULL) {// for only output transformations
        if(strcmp(rep , "LTF")==0 || strcmp(ns , "LTF_N")==0) {
            composed_ns = ns;
        }
        else if(strcmp(rep , "DFA")==0) {  
            composed_ns = ns;
        }
        else if(strcmp(rep , "DL")==0) {  
            composed_ns = ns;
        }
    }
    return composed_ns;
}

/**************************************************************************************** 
                        Functions to identify the input PUF construction
*****************************************************************************************/

/*void expandSerialParallelLoops() {

}*/

void printGraphNode(GraphNode* gnode) {
    // printf("==========================\nGraphNode : \n");
    // printf("  varName = %s\n", gnode->varName);
    // printf("  adjList = %d \t", (gnode->adjList!=NULL));
    // if(gnode->adjList!=NULL) {
    //     for(int j=0;j<gnode->outDeg;j++) {
    //         printf("-> %s", gnode->adjList[j]);
    //     }
    // }
    // printf("\n");
    // printf("  outDeg = %d\n", gnode->outDeg);
    // printf("  visited = %d\n", gnode->visited);
    // printf("  isPrimitive = %d\n", gnode->isPrimitive);
    // printf("  inLoop = %d\n==========================\n", gnode->inLoop);
}

void printGraph() {
    // printf("==========================printGraph==========================\n");
    // GraphStartNode* gsNode = headInitNode;
    // while(gsNode) {
    //     GraphNode* gnode = gsNode->initNode;
    //     while(gnode) {
    //         // printGraphNode(gnode);
    //         printf("\n%s  ", gnode->varName);
    //         if(gnode->adjList!=NULL) {
    //             for(int j=0;j<gnode->outDeg;j++) {
    //                 printf("-> %s", gnode->adjList[j]);
    //                 GraphNode* inode = gnode;
    //                 while(inode) {
    //                     // printf("\nin inode loop \t inode->varName = %s \t gnode->adjList[%d] = %s\n", inode->varName, j, gnode->adjList[j]);
    //                     if(strcmp(inode->varName, gnode->adjList[j])==0 && inode->visited==0) {
    //                         printf("adj[%d]\n", j);
    //                         printGraphNode(inode);
    //                         inode->visited=1;
    //                     }
    //                     inode = inode->nextGraphNode;
    //                 }
    //             }
    //         }
    //         gnode = gnode->nextGraphNode;
    //     }
    //     gsNode = gsNode->nextStartNode;
    // }
    
    // printf("\n======================end of printGraph=======================\n");
}

//checks statements of the form x = x + a[i]
int checkSelfAssignment(AssignmentNode* assignment) {
    // printf("------checkSelfAssignment------\n"); 
    char* assignVar = assignment->assignmentName;
    ExpressionNode* assignmentExpression = assignment->expressionPointer;
    // printf("--------in expressionPointer block before %s --------\n", evaluateExpression(assignmentExpression));
    if(assignmentExpression->leftChild && assignmentExpression->rightChild) {

        char* leftOp = evaluateExpression(assignmentExpression->leftChild);
        char* rightOp = evaluateExpression(assignmentExpression->rightChild);
        char* op = assignmentExpression->op;
        
        // DC : CHECK HERE WHY WE ARE GETTING A[I][I] INSTEAD OF A[I]  solution : change precedence of operators in grmmar rules, check grammar for normal form
        // printf("-----in expressionPointer block %s %s %s---\n", leftOp, op, rightOp);
        // printf("%s %d %d %d %d %d %d \n", assignVar, strcmp(assignVar, leftOp)==0, strstr(rightOp,"[")!=NULL, strstr(rightOp,"]")!=NULL, strcmp(assignVar, rightOp)==0, strstr(leftOp,"[")!=NULL, strstr(leftOp,"]")!=NULL);
        
        //comapres whether the left or the right operand has an iterator
        if((strcmp(assignVar, leftOp)==0 && strstr(rightOp,"[")!=NULL && strstr(rightOp,"]")!=NULL) || (strcmp(assignVar, rightOp)==0 && strstr(leftOp,"[")!=NULL && strstr(leftOp,"]")!=NULL)) {
            // printf("********in same variable loop*********\n");
            return 1;
        }
        else {
            // printf("in else inloop\n");
            return 0;
        }                
    }
}

/*
adds a new node to to the module graph (graph representation of a module)
typedef struct graphNode {
    char* varName; //variable name
    char* adjList; //list of variables connected to this node (dirwcted graph)
    int outDeg;
    int visited; //to check if the variable has been traversed or not
    int isPrimitive; // true if the variable is the output of a primitive call
    int inLoop;
    struct GraphNode* nextGraphNode;
}GraphNode;
*/

GraphNode* addGraphNodeWithValues(char* varName, char* nextVar, int outDeg, int visited, int isPrimitive, int inLoop) {
    printf("in addGraphNodeWithValues %s, in loop = %d\n", varName, inLoop);
    GraphNode* gnode;
    if(gnode = (GraphNode*)malloc(sizeof(GraphNode))) {
        // printf("\tassignmentName = %s\n", assignment->assignmentName);
        gnode->varName = varName;
        // gnode->adjList = NULL;
        //check if it is needed- next 2 lines - why not update adj in add edge?
        gnode->adjList = (char**)malloc(sizeof(char*));
        gnode->adjList[0] = nextVar;
        gnode->outDeg = outDeg;
        gnode->visited = visited;
        gnode->isPrimitive = isPrimitive;
        gnode->inLoop = inLoop;

        if(headGraphNode==NULL) {
            printf("in if addGraphNodeWithValues\n");
            headGraphNode = gnode;
        }
        else {
            printf("in else addGraphNodeWithValues\n");
            gnode->nextGraphNode = headGraphNode;
            headGraphNode = gnode;
        }    
    }
    printGraphNode(gnode);
}

void addGraphNode(AssignmentNode* assignment) {
    // printf("in addGraphNode %s, in loop = %d\n", assignment->assignmentName, (assignment->iteratorExpressionPointer!=NULL));
    GraphNode* gnode;
    GraphNode* itnode = headGraphNode;
    int nodeFound = 0;
    while(itnode) {
        if(strcmp(itnode->varName, assignment->assignmentName)==0)
            nodeFound = 1;
        itnode = itnode->nextGraphNode;
    }
    if(!nodeFound) {
        // gnode = addGraphNodeWithValues(assignment->assignmentName, NULL, 0, 0, ...);
        if(gnode = (GraphNode*)malloc(sizeof(GraphNode))) {
            // printf("\tassignmentName = %s\n", assignment->assignmentName);
            gnode->varName = assignment->assignmentName;
            gnode->adjList = NULL;
            gnode->outDeg = 0;
            gnode->visited = 0;
            gnode->isPrimitive = (assignment->primitiveCallPointer!=NULL);
            gnode->inLoop = (assignment->iteratorExpressionPointer!=NULL);

            if(!gnode->inLoop && assignment->expressionPointer) {
                int selfLoopExists = checkSelfAssignment(assignment);
                gnode->inLoop = selfLoopExists;
            }
            if(headGraphNode==NULL) {
                // printf("First addGraphNode\n");
                headGraphNode = gnode;
            }
            else {
                // printf("Adding another addGraphNode\n");
                gnode->nextGraphNode = headGraphNode;
                headGraphNode = gnode;
            }    
        }
        // printf("Calling print from addGraphNode\n");
        printGraphNode(gnode);
        // printGraph();
    }
}

void addEdge(AssignmentNode* assignment) {
    // printf("in addEdge lhs = %s \t inLoop = %d \t isExp = %d \t isPrimCall = %d \n", assignment->assignmentName, assignment->iteratorExpressionPointer!=NULL, assignment->expressionPointer!=NULL, assignment->primitiveCallPointer!=NULL);
    char* assignVar = assignment->assignmentName;

    //primitive call output assigned to a variable - add edges of the input variables of the primitive to the primitive output
    if(assignment->primitiveCallPointer) {
        char* inputdef = assignment->primitiveCallPointer->inputVariable;
        // printf("inputdef = %s\n", inputdef);
        
        char * inp_var = strtok(inputdef, ",");  //add the prim call in the adjacency list of the input params
        while( inp_var != NULL ) { // loop through the string to extract all other tokens
            int foundNode = 0;
            // printf(" ---- inp_var = %s\n", inp_var ); //printing each inp_var
            GraphStartNode* startNode = headInitNode;
            while(startNode) {
                GraphNode* gnode = startNode->initNode;
                /*
                  gnode - nodes corresponding to the module input params
                */
                while(gnode) {
                    // printf("\t\tin gnode loop - %s - %s\n", gnode->varName, inp_var);
                    if(strcmp(gnode->varName, inp_var)==0) {
                        // printf("----------------------found input_variable----------------------\n");
                        gnode->outDeg += 1;
                        char** outEdges = (char**)malloc(sizeof(char*)*(gnode->outDeg));
                        for(int i=0;i<gnode->outDeg-1;i++) {
                            outEdges[i] = gnode->adjList[i];
                        }
                        outEdges[gnode->outDeg-1] = assignVar;
                        gnode->adjList = outEdges;
                        // printf("Edge added to prim call output variable %s (%s -> %s)!!!! \n", assignVar, gnode->varName, assignVar);
                        foundNode = 1;
                        break;
                    }
                    gnode = gnode->nextGraphNode;
                }
                if(foundNode==1) //if input variable is found in a previous iteration, we need not check for other start nodes
                    break;
                startNode = startNode->nextStartNode;
            }
            inp_var = strtok(NULL, ",");
        }
    }
    if(assignment->expressionPointer) {
        // printf("assignment expression pointer block\n");
        // printExpressionTree(assignment->expressionPointer);
        char* expStr = evaluateExpression(assignment->expressionPointer);
        // printf("expStr = %s\t----------lhs = %d \t rhs = %d \n", expStr, assignment->expressionPointer->leftChild!=NULL, assignment->expressionPointer->rightChild!=NULL);
        
        //for statements with constant assignments - to-do: chnage - constants need not be only 0 or 1
        if(strcmp(expStr,"0")==0 || strcmp(expStr,"1")==0) { //dc_todo: change to all numbers from 0-9 using ascii
            // printf("------constant variable = %s------\n", expStr);
            // addGraphNode() for constant term;
            GraphNode* cgnode;
            if(cgnode = (GraphNode*)malloc(sizeof(GraphNode))) {
                cgnode->varName = expStr;
                cgnode->adjList = (char**)malloc(sizeof(char*));
                cgnode->adjList[0] = assignVar;
                cgnode->outDeg = 1;
                cgnode->visited = 0;
                cgnode->isPrimitive = 0;
                cgnode->inLoop = 0;
                // printf("Edge added: variable initialized (%s -> %s)!!!! \n", cgnode->varName, assignVar);
                if(headGraphNode==NULL) {
                    headGraphNode = cgnode;
                }
                else {
                    cgnode->nextGraphNode = headGraphNode;
                    headGraphNode = cgnode;
                }
            }
            // printf("Calling print from addEdge->expression\n");
            printGraphNode(cgnode);
        }
        else if(assignment->expressionPointer->leftChild || assignment->expressionPointer->rightChild) { // when there is an operation on the rhs
            // operation in rhs of assignment operation
            char* operator = assignment->expressionPointer->op;
            // printf("in addEdge -----------------------------------------------------------------------------------------operator = %s\n", operator);
            int foundNode = 0;
            GraphStartNode* startNode = headInitNode;
            while(startNode) {
                GraphNode* gnode = startNode->initNode;
                while(gnode) {         //gnode - nodes corresponding to the module input params
                    // printf("\t\tin gnode loop - %s\n", gnode->varName);
                    // checking for an indexing operation
                    if(strcmp(operator,"[")==0 && strstr(expStr, gnode->varName)) {
                        // printf("----------------------found input_variable----------------------\n");
                        gnode->outDeg += 1;
                        char** outEdges = (char**)malloc(sizeof(char*)*(gnode->outDeg));
                        for(int i=0;i<gnode->outDeg-1;i++) {
                            outEdges[i] = gnode->adjList[i];
                        }
                        outEdges[gnode->outDeg-1] = assignVar;
                        gnode->adjList = outEdges;
                        // printf("Edge added %s -> %s!!!! \n", gnode->varName, assignVar);
                        foundNode = 1;
                        break;
                    }
                    else {
                        // printf("other operation !!!!! \n");
                        if(assignment->expressionPointer->leftChild) {
                            char* leftExp = evaluateExpression(assignment->expressionPointer->leftChild);
                            // printf("leftExp = %s\n", leftExp);
                        }
                    }
                    gnode = gnode->nextGraphNode;
                }
                if(foundNode==1) //if input variable is found in a previous iteration, we need not check for other start nodes
                    break;
                startNode = startNode->nextStartNode;
            }
        }
        else{ //only single variable in rhs
            // printf("in addedge - variable in rhs assignemnt\n");
            //use the expressionNode to find the operands and operator
            // printExpressionTree(assignment->expressionPointer);
            printGraph();
            char* operator = assignment->expressionPointer->op;
            // printf("\n rhs variable = %s \t %s\n", operator, expStr);
            int foundNode = 0;
            GraphStartNode* startNode = headInitNode;
            while(startNode) {
                // printf("\t\tstartnode\n");
                GraphNode* gnode = startNode->initNode;
                while(gnode) {         //gnode - nodes corresponding to the module input params
                    // printf("\n%s  ", gnode->varName);
                    // printf("\t\tin gnode loop - %s\n", gnode->varName);
                    if(strcmp(operator, gnode->varName)==0) { //dc: check for substring instead of exact match.. or better left part of [] in case of indexing
                        // printf("----------------------found input_variable----------------------\n");
                        gnode->outDeg += 1;
                        char** outEdges = (char**)malloc(sizeof(char*)*(gnode->outDeg));
                        for(int i=0;i<gnode->outDeg-1;i++) {
                            outEdges[i] = gnode->adjList[i];
                        }
                        outEdges[gnode->outDeg-1] = assignVar;
                        gnode->adjList = outEdges;
                        // printf("Edge added %s -> %s!!!! \n", gnode->varName, assignVar);
                        foundNode = 1;
                        break;
                    }
                    gnode = gnode->nextGraphNode;
                }
                if(foundNode==1) //if input variable is found in a previous iteration, we need not check for other start nodes
                    break;
                startNode = startNode->nextStartNode;
            }
        }
    }
}

//nested function to identify structure
/*char* find_submodule_sub (StatementNode* statement) {
    // printf("find_submodule_sub %d\n", statement!=NULL);
    char *prim = NULL;
    char* inputVar = NULL;
    //base case
    if(statement->assignmentPointer && statement->assignmentPointer->primitiveCallPointer) {
        prim = statement->assignmentPointer->primitiveCallPointer->primitivePointer;
        inputVar = statement->assignmentPointer->primitiveCallPointer->inputVariable;
        // printf("in find_submodule_sub base case %s(%s)\n", prim, inputVar);
        return prim;
    }
    while(statement) {
        // printf("in statement loop %d \n", (statement->assignmentPointer!=NULL));

        if(statement->assignmentPointer && statement->assignmentPointer->primitiveCallPointer) {
            prim = statement->assignmentPointer->primitiveCallPointer->primitivePointer;
            inputVar = statement->assignmentPointer->primitiveCallPointer->inputVariable;
            printf("==================================================================================>in primitive calls statement %s(%s)\n", prim, inputVar);
            break;
        }
        else if(statement->ifElsePointer) {
            StatementNode *st = statement->ifElsePointer->ifStatementPointer;
            while(st) {
                prim = find_submodule_sub(st);
                if(prim!=NULL)
                    return prim;
                st = st->nextStatement;
            }
            st = statement->ifElsePointer->elseStatementPointer;
            while(st) {
                prim = find_submodule_sub(st);
                if(prim!=NULL)
                    return prim;
                st = st->nextStatement;
            }
        }
        else if(statement->serialPointer) {
            StatementNode *st = statement->serialPointer->serialStatementPointer;
            while(st) {
                prim = find_submodule_sub(st);
                if(prim!=NULL)
                    return prim;
                st = st->nextStatement;
            }
        }
        else if(statement->parallelPointer) {
            StatementNode *st = statement->parallelPointer->parallelStatementPointer;
            while(st) {
                prim = find_submodule_sub(st);
                if(prim!=NULL)
                    return prim;
                st = st->nextStatement;
            }
        }
        statement = statement->nextStatement;
    }
    return NULL;
}

ModuleNode* find_submodule(ModuleNode* module) {    //wrapper of recursive calls to find_submodule
    StatementNode *statement = module->statementPointer;
    char *prim = find_submodule_sub(statement); //called recursively
    // printf("after find_submodule_sub\n");
    ModuleNode* mod = headModuleNode;
    while(mod) {
        if(strcmp(mod->primitivePointer, prim)==0)
            return mod;     //return the submodule
        mod= mod->nextModule;
    }
    // prints if no submodule of given name found - module name and primitive call name mismatch
    printf("after while loop in find_submodule  %s\n", module->primitivePointer); 
    return NULL;
}*/

void createGraphStartNodes(InputDefNode* input) {
    printf("-------------in createGraphStartNodes-------------\n");
    GraphNode* gnode;
    while(input) {
        if((gnode = (GraphNode*)malloc(sizeof(GraphNode)))!=NULL) {
            // printf("variableName = %s\n", input->tuple);
            gnode->varName = input->tuple;
            gnode->adjList = NULL;
            gnode->outDeg = 0;
            gnode->visited = 0;
            gnode->isPrimitive = 0;
            
            //indicates that the assignment statement lies in a series or parallel block
            gnode->inLoop = 0;
            if(headInitNode==NULL) {
                // printf("in if\n");
                headInitNode = (GraphStartNode*)malloc(sizeof(GraphStartNode));
                headInitNode->initNode = gnode;
                headInitNode->nextStartNode = NULL;
            }
            else {
                // printf("in else\n");
                GraphStartNode* startNode = (GraphStartNode*)malloc(sizeof(GraphStartNode));
                startNode->initNode = gnode;
                startNode->nextStartNode = headInitNode;
                headInitNode = startNode;
            }
        }
        input = input->nextInputDef;    
    }
    // printf("After adding module input params : \n*******************************************************\nGraphStartNodes ");
    // //print start nodes
    // GraphStartNode* startNode = headInitNode;
    // while(startNode) {
    //     printf("==> %s ", startNode->initNode->varName); //prints the name of the variable (vertex name)
    //     startNode = startNode->nextStartNode;
    // }
    // printf("\n*******************************************************\nAFTER ADDING NODES CORREPSONDING TO INPUT PARAMETERS IN GRAPH \n\n");
}

void createGraphNode() {

}

void createGraph() {

}

/*
  nested function to identify structure - 
  function idenifies/picks the primitive calls in a module definition
  adds new primitive to primList - which consists of all primitives and their respective module definition.
*/

void findSubmodulesSub(StatementNode* statement, PrimNode** primList) {
    printf("findSubmodulesSub %d, %d\t\n", statement!=NULL, *primList!=NULL);
    /*if(statement) {
        printf("as = %d\t ifelse = %d\t ser = %d\t par = %d\n", statement->assignmentPointer!=NULL, statement->ifElsePointer!=NULL, statement->serialPointer!=NULL, statement->parallelPointer!=NULL);
        if(statement->assignmentPointer!=NULL) {
            printf("%s\n", statement->assignmentPointer->assignmentName);
        }
    }*/
    char *primName = NULL;
    char* inputVar = NULL;

    while(statement) {
        // printf("in statement loop as = %d, ifs = %d, ser = %d, par = %d\n", (statement->assignmentPointer!=NULL), (statement->ifElsePointer!=NULL), (statement->serialPointer!=NULL), (statement->parallelPointer!=NULL));
        if(statement->assignmentPointer) { 
            //check for primitive calls 
            if(statement->assignmentPointer->primitiveCallPointer) {
                primName = statement->assignmentPointer->primitiveCallPointer->primitivePointer;
                inputVar = statement->assignmentPointer->primitiveCallPointer->inputVariable;
                // printf("in findSubmodulesSub base case %s(%s)\n", primName, inputVar);
                PrimNode* newPrim = (PrimNode*)malloc(sizeof(PrimNode));
                newPrim->primitiveName = primName;
                newPrim->inputVariable = inputVar;
                newPrim->modulePointer = NULL;
                newPrim->rep = "-";
                newPrim->nextPrim = NULL;

                // To find the corresponding module pointer and add to the PrimNode structure
                ModuleNode* moduleIter = headModuleNode;
                while(moduleIter) {
                    // printf("----------in moduleIter while %s compared to %s \n", moduleIter->primitivePointer, primName);
                    if(strcmp(moduleIter->primitivePointer, primName)==0) {
                        newPrim->modulePointer = moduleIter;
                        break;
                    }
                    moduleIter = moduleIter->nextModule;
                }
                
                // add the new prim at the end of primList
                // PrimNode* primListIter = *primList;
                // while(primListIter && primListIter->nextPrim)
                //     primListIter = primListIter->nextPrim;
                // primListIter->nextPrim = newPrim;

                // appending node at the beginning
                newPrim->nextPrim = *primList;
                *primList = newPrim;
                printf("=================================after adding %s(%s) to the primList\n", primName, inputVar);
            }
            // new functions added on 14 aug - not usign graph functions now
            // addGraphNode(statement->assignmentPointer);
            // addEdge(statement->assignmentPointer);
        }
        else if(statement->ifElsePointer) {
            StatementNode *st = statement->ifElsePointer->ifStatementPointer;
            while(st) {
                findSubmodulesSub(st, primList);
                st = st->nextStatement;
            }

            st = statement->ifElsePointer->elseStatementPointer;
            while(st) {
                findSubmodulesSub(st, primList);
                st = st->nextStatement;
            }
        }
        else if(statement->serialPointer) {
            StatementNode *st = statement->serialPointer->serialStatementPointer;
            while(st) {
                findSubmodulesSub(st, primList);
                st = st->nextStatement;
            }
        }
        else if(statement->parallelPointer) {
            StatementNode *st = statement->parallelPointer->parallelStatementPointer;
            while(st) {
                findSubmodulesSub(st, primList);
                st = st->nextStatement;
            }
        }
        statement = statement->nextStatement;
    }
    return;
}

//returns a list of submodules in a given input module
void findSubmodules(ModuleNode* module, PrimNode** primList) {    //wrapper of recursive calls to find_submodule_sub
    printf("findSubmodules %s\n", module->primitivePointer);
    StatementNode *statement = module->statementPointer;

    // add vertices for module formal parameters - dc: replace this with createGraphStartNode() function call
    createGraphStartNodes(module->inputDefPointer);

    // to identify the submodules, traverse the statements of the module
    statement = module->statementPointer;
    findSubmodulesSub(statement, primList); //called recursively
    // printf("after findSubmodulesSub of %s  SUBMODULES : \n ", module->primitivePointer);
    // PrimNode *prim = *primList;
    // while(prim) {
    //     printf("====> %s (%s) \t module = %d\n", prim->primitiveName, prim->inputVariable, (prim->modulePointer!=NULL));
    //     prim = prim->nextPrim;
    // }
    printGraph();
    
    // printf("after while loop in find_submodule  %s\n", module->primitivePointer); 
}

int isInterposition(ModuleNode* module, ModuleNode* submod) {
    // char* modInputs = getModuleInputs(module);
    // char* submodInputs = getModuleInputs(submod);
    // printf("-----> in isInterposition %s(%s) - %s(%s)\n", module->primitivePointer, modInputs, submod->primitivePointer, submodInputs);
    printf("-----> in isInterposition %s - %s\n", module->primitivePointer, submod->primitivePointer);
    StatementNode *st = module->statementPointer;
    char *prim = NULL;
    int first_xor = 0, second_xor = 0, chal_assign = 0;//, chal_assign_2 = 0;
    char* interpose_bit = NULL;
    int set_interpose = 0;
    while(st) {
        // printf("----------------in while as = %d, ifs = %d, ser = %d, par = %d \n", (st->assignmentPointer!=NULL), (st->ifElsePointer!=NULL), (st->serialPointer!=NULL), (st->parallelPointer!=NULL));
        if(st->assignmentPointer) {
            if(st->assignmentPointer->primitiveCallPointer) {
                prim = st->assignmentPointer->primitiveCallPointer->primitivePointer;
                // printf("primitive call in isInterposition - %s\n", prim);
                if(strcmp(prim, submod->primitivePointer)==0) {
                    if(!first_xor) {
                        // printf("Found first XOR\n");
                        first_xor = 1;
                        interpose_bit = st->assignmentPointer->assignmentName;  //response from first xorpuf
                        // printf("interpose_bit = %s\n", interpose_bit);  //interpose variable name
                    }
                    else {
                        printf("Found more than one XOR with input (%s)\n", st->assignmentPointer->primitiveCallPointer->inputVariable);
                        second_xor = 1;
                    }
                }
            }
            if(st->assignmentPointer->expressionPointer) {
                ExpressionNode *exp = st->assignmentPointer->expressionPointer;
                if(interpose_bit!=NULL && strcmp(exp->op,interpose_bit)==0) {
                    // printf("in if\n");
                    set_interpose = 1;
                }
            }
        }
        if(st->parallelPointer) {
            if(first_xor) {
                // printf("parallel block after first xor\n");
                ExpressionNode *exp = st->parallelPointer->parallelExpressionPointer;
                if(strstr(exp->op,"s")!=NULL) {//check with s param
                    // printf("in s if\n");
                    chal_assign = 1;
                }
            }
        }
        st = st->nextStatement;
    }
    if(first_xor && second_xor && chal_assign && set_interpose) {
        printf("\t isInterposition return 1\n");
        return 1;
    }
    printf("\t isInterposition return 0\n");
    return 0;
}

int IsFeedForward(ModuleNode* module, ModuleNode* submod) {
    printf("-----> in IsFeedForward %s - %s\n", module->primitivePointer, submod->primitivePointer);
    StatementNode *st = module->statementPointer;
    int set_ffin = 0, set_ffout = 0, set_switches = 0;
    char* var = NULL;
    while(st) {
        // printf("----------------in while as = %d, ifs = %d, ser = %d, par = %d \n", (st->assignmentPointer!=NULL), (st->ifElsePointer!=NULL), (st->serialPointer!=NULL), (st->parallelPointer!=NULL));
        if(st->assignmentPointer && st->assignmentPointer->primitiveCallPointer) {
            char* prim = st->assignmentPointer->primitiveCallPointer->primitivePointer;
            if(strcmp(prim, "SWITCH_2X2")==0) {
                // printf("--Switch initialized\n");
                set_switches = 1;
                printf("%s\n", st->assignmentPointer->assignmentName);
            }
        }
        if(st->serialPointer) {
            StatementNode* sst = st->serialPointer->serialStatementPointer;
            while(sst) {
                if(sst->ifElsePointer && sst->ifElsePointer->ifExpressionPointer && sst->ifElsePointer->ifStatementPointer) {
                    ExpressionNode* if_exp = sst->ifElsePointer->ifExpressionPointer;
                    char* ifexp = evaluateExpression(if_exp);
                    // printf("--ifexp = %s\n", ifexp);
                    if(sst->ifElsePointer->ifStatementPointer->assignmentPointer && sst->ifElsePointer->ifStatementPointer->assignmentPointer->primitiveCallPointer) { //strstr("ff_in")!=NULL
                        // printf("--ff_in assignment statement\n");
                        char* prim = sst->ifElsePointer->ifStatementPointer->assignmentPointer->primitiveCallPointer->primitivePointer;
                        if(strcmp(prim,"ARBITER")==0) {
                            set_ffin = 1;
                        }
                        // printf("prim = %s \t set_ffin = %d\n", prim, set_ffin);
                        var = sst->ifElsePointer->ifStatementPointer->assignmentPointer->assignmentName;
                    }
                    else if(sst->ifElsePointer->ifStatementPointer->assignmentPointer && sst->ifElsePointer->ifStatementPointer->assignmentPointer->expressionPointer) {
                        // printf("--ff_out assignment statement\n");
                        ExpressionNode* rhs = sst->ifElsePointer->ifStatementPointer->assignmentPointer->expressionPointer;
                        char* ffout_chal = evaluateExpression(rhs);     //stores teh variable name used in rhs of second if block
                        if(strcmp(ffout_chal,var)==0) {
                            set_ffout = 1;
                        }
                    }
                }
                sst = sst->nextStatement;
            }
        }
        
        st = st->nextStatement;
    }
    // printf("set_switches = %d \t set_ffin = %d \t set_ffout = %d \n", set_switches, set_ffin, set_ffout);
    if(set_switches && set_ffin && set_ffout) {
        printf("---------------------------------------------------> IsFeedForward return 1\n");
        return 1;
    }
    printf("IsFeedForward return 0\n");
    return 0;
}

//so far we are assuming fixed reccurence architecture.. n/m consecutive bit positions xored with same response bit and theta=1
int IsRecurrentLoop(ModuleNode* module, ModuleNode* submod) {
    printf("-----> in IsRecurrentLoop %s - %s\n", module->primitivePointer, submod->primitivePointer);
    StatementNode *st = module->statementPointer;
    char *prim = NULL, *first_prim = NULL, *second_prim = NULL;
    char* internal_challenge = NULL, *internal_response = NULL;    
    int first_puf=0, second_puf=0, init_internal_challenge = 0, chal_modification = 0; 
    int rec_loop = 0;

    char* external_challenge = getPUFChallenge(module->inputDefPointer);
    
    while(st) {
        // printf("----------------in while as = %d, ifs = %d, ser = %d, par = %d \n", (st->assignmentPointer!=NULL), (st->ifElsePointer!=NULL), (st->serialPointer!=NULL), (st->parallelPointer!=NULL));
        if(st->assignmentPointer) {
            if(st->assignmentPointer->primitiveCallPointer) {

                prim = st->assignmentPointer->primitiveCallPointer->primitivePointer;
                // printf("primitive call in IsRecurrentLoop - %s\n", prim);
                if(strcmp(prim, submod->primitivePointer)==0) {
                    if(!first_puf) {
                        first_puf = 1;
                        first_prim = prim;
                        // printf("Found first PUF %s \n", first_prim);
                        internal_response = st->assignmentPointer->assignmentName;  //response of core puf
                        // printf("internal_response = %s\n", internal_response);  //interpose variable name
                    }
                    else {
                        second_puf = 1;
                        second_prim = prim;
                        char* submod_challenge = st->assignmentPointer->primitiveCallPointer->inputVariable;
                        // printf("Found more than one PUF call %s \t\t\t\t %s \t %s\n", second_prim, submod_challenge, internal_challenge);
                        if(strstr(submod_challenge, internal_challenge)) {
                            rec_loop = 1;
                        }
                    }
                }
            }
            if(st->assignmentPointer->expressionPointer) {
                ExpressionNode *exp = st->assignmentPointer->expressionPointer;
                if(strcmp(exp->op,external_challenge)==0) {
                    // printf("init internal challenge\n");
                    init_internal_challenge = 1;
                    internal_challenge = st->assignmentPointer->assignmentName;
                    // printf("internal_challenge = %s\n", internal_challenge);
                }
            }
        }
        if(st->parallelPointer) {
            StatementNode* sst = st->parallelPointer->parallelStatementPointer;
            while(sst) {
                if(sst->parallelPointer) {
                    StatementNode* ssst = sst->parallelPointer->parallelStatementPointer;
                    while(ssst) {
                        if(ssst->assignmentPointer->expressionPointer) {
                            ExpressionNode *exp = ssst->assignmentPointer->expressionPointer;
                            char* leftOperand = evaluateExpression(exp->leftChild);
                            char* rightOperand = evaluateExpression(exp->rightChild);
                            if(strstr(leftOperand, external_challenge) && strcmp(exp->op, "XOR")==0 && strstr(rightOperand,internal_response)) {
                                chal_modification = 1;
                                // internal_challenge = ssst->assignmentPointer->assignmentName;
                            }
                        }
                        ssst = ssst->nextStatement;
                    }    
                }
                sst = sst->nextStatement;
            }
        }
        st = st->nextStatement;
    }
    if(first_puf && chal_modification && rec_loop) {
        printf("\t IsRecurrentLoop return 1 %d, %d, %d\n", first_puf, chal_modification, rec_loop);
        return 1;
    }
    printf("\t IsRecurrentLoop return 0\n");
    return 0;
}

char* identifyInputTransformation(ModuleNode* module, ModuleNode* submod) {
    printf("\n-----identifyInputTransformation \t mod = %s \t submod = %s\n", module->primitivePointer, submod->primitivePointer);
    char *input_trans_type = NULL;
    int has_ff_input=0, has_ff_output=0, has_ibit=0;

    if(isInterposition(module, submod)) {
        input_trans_type = "interpose";
        // printf("--input_trans_type = %s\n", input_trans_type);
    }
    else if(IsFeedForward(module, submod)) {
        input_trans_type = "feed-forward";
    }
    else if(IsRecurrentLoop(module, submod)) {
        input_trans_type = "recurrent-loop";
    }
    printf("input_trans_type = %s\n", input_trans_type );
    return input_trans_type;
}

/*int IsXOR (ModuleNode* module, ModuleNode* submod) {    //combined using XOR gate(s)
    printf("-----> in isXOR %s - %s\n", module->primitivePointer, submod->primitivePointer);
    StatementNode *st = module->statementPointer;
    int parallel = 0, has_xor = 0, init_xor = 0;
    char* output_var;
    char* init_var;
    while(st) {
        // printf("----------------in while as = %d, ifs = %d, ser = %d, par = %d \n", (st->assignmentPointer!=NULL), (st->ifElsePointer!=NULL), (st->serialPointer!=NULL), (st->parallelPointer!=NULL));
        if(st->parallelPointer) {
            StatementNode* pst = st->parallelPointer->parallelStatementPointer;
            while(pst) {
                if(pst->assignmentPointer && pst->assignmentPointer->primitiveCallPointer) {
                    char *prim = pst->assignmentPointer->primitiveCallPointer->primitivePointer;
                    if(strcmp(prim, submod->primitivePointer)==0) { //strcmp(submod->primitivePointer, "APUF")==0 && 
                        // printf(" Submodule in parallel block\n");
                        parallel = 1;
                    }
                }
                pst = pst->nextStatement;
            }
        }
        if(st->assignmentPointer && st->assignmentPointer->expressionPointer) {
            output_var = st->assignmentPointer->assignmentName;
            // printf("output_var = %s \t %d\n", output_var, st->assignmentPointer->expressionPointer!=NULL);
            init_var = st->assignmentPointer->expressionPointer->op;
            if(strcmp(init_var, "1")==0) {
                // printf("set init_xor = 1\n");
                init_xor = 1;
            }
        }
        if(st->serialPointer) {
            StatementNode* sst = st->serialPointer->serialStatementPointer;
            while(sst) {
                if(sst->assignmentPointer && sst->assignmentPointer->expressionPointer) {
                    char* operator = sst->assignmentPointer->expressionPointer->op;
                    if(strcmp(operator, "XOR")==0)
                        has_xor = 1;
                }
                sst = sst->nextStatement;
            }
        }
        st = st->nextStatement;
    }
    // printf("parallel = %d\t init_xor = %d \t has_xor = %d \n", parallel, init_xor, has_xor);
    if(parallel && init_xor && has_xor) {
        printf("\t IsXOR return 1\n");
        return 1;
    }
    printf("\t IsXOR return 0\n");
    return 0;
}*/

int IsXOR (ModuleNode* module, ModuleNode* submod) {    //combined using XOR gate(s)
    printf("-----> in isXOR %s - %s\n", module->primitivePointer, submod->primitivePointer);
    StatementNode *st = module->statementPointer;
    int parallel = 0, has_xor = 0, init_xor = 0;
    char* output_var;
    char* init_var;
    while(st) {
        // printf("----------------in while as = %d, ifs = %d, ser = %d, par = %d \n", (st->assignmentPointer!=NULL), (st->ifElsePointer!=NULL), (st->serialPointer!=NULL), (st->parallelPointer!=NULL));
        if(st->parallelPointer) {
            StatementNode* pst = st->parallelPointer->parallelStatementPointer;
            while(pst) {
                if(pst->assignmentPointer && pst->assignmentPointer->primitiveCallPointer) {
                    char *prim = pst->assignmentPointer->primitiveCallPointer->primitivePointer;
                    if(strcmp(prim, submod->primitivePointer)==0) { //strcmp(submod->primitivePointer, "APUF")==0 && 
                        // printf(" Submodule in parallel block\n");
                        parallel = 1;
                    }
                }
                pst = pst->nextStatement;
            }
        }
        if(st->assignmentPointer && st->assignmentPointer->expressionPointer) {
            output_var = st->assignmentPointer->assignmentName;
            // printf("output_var = %s \t %d\n", output_var, st->assignmentPointer->expressionPointer!=NULL);
            init_var = st->assignmentPointer->expressionPointer->op;
            if(strcmp(init_var, "1")==0 || strcmp(init_var, "0")==0) {
                // printf("set init_xor = 1\n");
                init_xor = 1;
            }
        }
        if(st->serialPointer) {
            StatementNode* sst = st->serialPointer->serialStatementPointer;
            while(sst) {
                if(sst->assignmentPointer && sst->assignmentPointer->expressionPointer) {
                    char* operator = sst->assignmentPointer->expressionPointer->op;
                    if(strcmp(operator, "XOR")==0)
                        has_xor = 1;
                }
                sst = sst->nextStatement;
            }
        }
        st = st->nextStatement;
    }
    // printf("parallel = %d\t init_xor = %d \t has_xor = %d \n", parallel, init_xor, has_xor);
    if(parallel && init_xor && has_xor) {
        printf("\t IsXOR return 1\n");
        return 1;
    }
    printf("\t IsXOR return 0\n");
    return 0;
}

int IsMUX (ModuleNode* module, ModuleNode* submod) {    //combined using MUX network
    printf("-----> in isMUX %s - %s\n", module->primitivePointer, submod->primitivePointer);
    StatementNode *st = module->statementPointer;
    int set_data_lines = 0, set_select_lines = 0, has_mux_network = 0;
    while(st) {
        // printf("----------------in while as = %d, ifs = %d, ser = %d, par = %d \n", (st->assignmentPointer!=NULL), (st->ifElsePointer!=NULL), (st->serialPointer!=NULL), (st->parallelPointer!=NULL));
        if(st->parallelPointer) {
            ExpressionNode* exp = st->parallelPointer->parallelExpressionPointer;
            if(strcmp(exp->op, "k")==0) {
                StatementNode* pst = st->parallelPointer->parallelStatementPointer;
                while(pst) {
                    if(pst->serialPointer) {
                        char* exp_all = evaluateExpression(pst->serialPointer->serialExpressionPointer);
                        // printf("exp_all = %s\n", exp_all);
                        if(strstr(exp_all,"k")!=NULL) {
                            StatementNode* sst = pst->serialPointer->serialStatementPointer;
                            while(sst) {
                                // printf("---> inside serial block\n");
                                if(sst->assignmentPointer!=NULL && sst->assignmentPointer->primitiveCallPointer!=NULL) {
                                    char *prim = sst->assignmentPointer->primitiveCallPointer->primitivePointer;
                                    // printf("prim = %s\n", prim);
                                    if(strcmp(prim, "MUX_2X1")==0) {
                                        has_mux_network = 1;
                                    }
                                }
                                sst = sst->nextStatement;
                            }
                        }
                    }
                    else if(pst->assignmentPointer && pst->assignmentPointer->primitiveCallPointer) {
                        char *prim = pst->assignmentPointer->primitiveCallPointer->primitivePointer;
                        if(strcmp(prim, submod->primitivePointer)==0) {
                            // printf(" set_select_lines \n");
                            set_select_lines = 1;
                        }
                    }
                    pst = pst->nextStatement;
                }
            }
            else if(strstr(exp->op, "k")!=NULL) {    //change to 2^k
                // printf("data chains\n");
                StatementNode* pst = st->parallelPointer->parallelStatementPointer;
                while(pst) {
                    if(pst->assignmentPointer && pst->assignmentPointer->primitiveCallPointer) {
                        char *prim = pst->assignmentPointer->primitiveCallPointer->primitivePointer;
                        if(strcmp(prim, submod->primitivePointer)==0) {
                            // printf(" set_data_lines \n");
                            set_data_lines = 1;
                        }
                    }
                    pst = pst->nextStatement;
                }
            }
        }
        st = st->nextStatement;
    }
    // printf("set_select_lines = %d \t set_data_lines = %d \t has_mux_network = %d \n", set_select_lines, set_data_lines, has_mux_network);
    if(set_data_lines && set_select_lines && has_mux_network) {
        printf("\t IsMUX return 1\n");
        return 1;
    }
    printf("\t IsMUX return 0\n");
    return 0;
}

int hasCrossedPaths (ModuleNode* module, ModuleNode* submod) {  //as in case of DAPUF
    printf("-----> in hasCrossedPaths %s - %s\n", module->primitivePointer, submod->primitivePointer);
    int parallel_delay_chain = 0, top_arbiters = 0, bottom_arbiters = 0, top_xor = 0, bottom_xor = 0;
    StatementNode *st = module->statementPointer;
    char* top_arb = NULL, *bottom_arb = NULL;
    while(st) {
        // printf("----------------in while as = %d, ifs = %d, ser = %d, par = %d \n", (st->assignmentPointer!=NULL), (st->ifElsePointer!=NULL), (st->serialPointer!=NULL), (st->parallelPointer!=NULL));
        if(st->parallelPointer) {
            StatementNode* pst = st->parallelPointer->parallelStatementPointer;
            while(pst) {
                if(pst->assignmentPointer) {
                    if(pst->assignmentPointer->primitiveCallPointer) { //to check for delay chains
                        char *prim = pst->assignmentPointer->primitiveCallPointer->primitivePointer;
                        if(strcmp(submod->primitivePointer, "DELAY_CHAIN")==0 && strcmp(prim, submod->primitivePointer)==0) {
                            // printf("DELAY_CHAIN modules in parallel block\n");
                            parallel_delay_chain = 1;
                        }
                    }
                    else { //to check for xors
                        ExpressionNode *exp = pst->assignmentPointer->expressionPointer;
                        if(strcmp(exp->op, "XOR")==0) {
                            // printf("XOR gate in parallel block\n");
                            if(top_xor==0) {
                                top_arb = evaluateExpression(exp->rightChild);
                                // printf("top_arb = %s\n", top_arb);
                                top_xor = 1;
                            }
                            else if(top_xor==1 && bottom_xor==0) {
                                bottom_arb = evaluateExpression(exp->rightChild);
                                // printf("bottom_arb = %s\n", bottom_arb);
                                bottom_xor = 1;
                            }
                        }
                    }
                }
                pst = pst->nextStatement;
            }
            //add conditions to check series of xors
        }
        else if(st->serialPointer) {
            StatementNode* sst = st->serialPointer->serialStatementPointer;
            while(sst) {
                if(sst->parallelPointer) {
                    // printf("Parallel block inside serial block\n");
                    StatementNode* pst = sst->parallelPointer->parallelStatementPointer;
                    while(pst) {
                        // printf("inside parallelblock pst %d \t %d\n", pst->assignmentPointer!=NULL, pst->assignmentPointer->primitiveCallPointer!=NULL);
                        if(pst->assignmentPointer && pst->assignmentPointer->primitiveCallPointer) {
                            char* prim = pst->assignmentPointer->primitiveCallPointer->primitivePointer;
                            // printf("prim = %s\n", prim);
                            if(strcmp(prim, "ARBITER")==0) {
                                // printf("ARBITER modules in parallel block\n");
                                if(top_arbiters==0) {
                                    top_arbiters = 1;
                                    top_arb = pst->assignmentPointer->assignmentName;
                                }
                                else {
                                    bottom_arbiters = 1;
                                    bottom_arb = pst->assignmentPointer->assignmentName;
                                }
                                // printf("top_arbiters = %d \t bottom_arbiters = %d \n", top_arbiters, bottom_arbiters);
                            }
                        }
                        pst = pst->nextStatement;
                    }
                }
                sst = sst->nextStatement;
            }
        }
        st = st->nextStatement;
    }
    // printf("parallel_delay_chain = %d \t top_arbiters = %d \t bottom_arbiters = %d \t top_xor = %d \t bottom_xor = %d\n", parallel_delay_chain, top_arbiters, bottom_arbiters, top_xor, bottom_xor);
    if(parallel_delay_chain && top_arbiters && bottom_arbiters && top_xor && bottom_xor) {
        printf("\t hasCrossedPaths return 1\n");
        return 1;
    }
    printf("\t hasCrossedPaths return 0\n");
    return 0;
}

int isBentFunction  (ModuleNode* module, ModuleNode* submod) {    //combined using Bent Function
    printf("-----> in isBentFunction %s - %s\n", module->primitivePointer, submod->primitivePointer);
    StatementNode *st = module->statementPointer;
    int parallel = 0, has_xor = 0, init_xor = 0;
    char* output_var;
    char* init_var;
    // while(st) {
    //     // printf("----------------in while as = %d, ifs = %d, ser = %d, par = %d \n", (st->assignmentPointer!=NULL), (st->ifElsePointer!=NULL), (st->serialPointer!=NULL), (st->parallelPointer!=NULL));
    //     if(st->parallelPointer) {
    //         StatementNode* pst = st->parallelPointer->parallelStatementPointer;
    //         while(pst) {
    //             if(pst->assignmentPointer && pst->assignmentPointer->primitiveCallPointer) {
    //                 char *prim = pst->assignmentPointer->primitiveCallPointer->primitivePointer;
    //                 if(strcmp(prim, submod->primitivePointer)==0) { //strcmp(submod->primitivePointer, "APUF")==0 && 
    //                     // printf(" Submodule in parallel block\n");
    //                     parallel = 1;
    //                 }
    //             }
    //             pst = pst->nextStatement;
    //         }
    //     }
    //     if(st->assignmentPointer && st->assignmentPointer->expressionPointer) {
    //         output_var = st->assignmentPointer->assignmentName;
    //         // printf("output_var = %s \t %d\n", output_var, st->assignmentPointer->expressionPointer!=NULL);
    //         init_var = st->assignmentPointer->expressionPointer->op;
    //         if(strcmp(init_var, "1")==0) {
    //             // printf("set init_xor = 1\n");
    //             init_xor = 1;
    //         }
    //     }
    //     if(st->serialPointer) {
    //         StatementNode* sst = st->serialPointer->serialStatementPointer;
    //         while(sst) {
    //             if(sst->assignmentPointer && sst->assignmentPointer->expressionPointer) {
    //                 char* operator = sst->assignmentPointer->expressionPointer->op;
    //                 if(strcmp(operator, "XOR")==0)
    //                     has_xor = 1;
    //             }
    //             sst = sst->nextStatement;
    //         }
    //     }
    //     st = st->nextStatement;
    // }
    // printf("parallel = %d\t init_xor = %d \t has_xor = %d \n", parallel, init_xor, has_xor);
    // if(parallel && init_xor && has_xor) {
    //     printf("\t IsXOR return 1\n");
    //     return 1;
    // }
    // printf("\t IsXOR return 0\n");
    // return 0;
}

char* identifyOutputTransformation (ModuleNode *module, ModuleNode *submod) {
    printf("\n-----identifyOutputTransformation \t mod = %s \t submod = %s\n", module->primitivePointer, submod->primitivePointer);
    // printf("-----identifyOutputTransformation\n");
    char* output_trans_type = NULL;

    int has_xor=0, has_mux=0;//, has_ibit=0;
    int has_delay_chain=0, has_double_arb=0;
    if(IsXOR(module, submod)) {
        output_trans_type = "xor";
        // printf("in xor output\n");
    }
    else if(IsMUX(module, submod)) {
        output_trans_type = "mux";
        // printf("in mux output\n");
    }
    else if(hasCrossedPaths(module, submod)) {
        output_trans_type = "dapuf";
        // printf("in dapuf output\n");
    }
    printf("output_trans_type = %s\n", output_trans_type);
    return output_trans_type;
}

// Ideally it should identify the connections using graph edges - currently tested for interpose compositions
char* check_submodule_connection_2 (ModuleNode *module, ModuleNode *currentSubmodule, ModuleNode *nextSubmodule) {
    printf("check_submodule_connection_2\n");
    return "interpose";
}

/**************************************************************************************** 
                        end of PUF identification functions 
*****************************************************************************************/

char* identifyModule (ModuleNode *module, int PACSetting) {
    printf("-------- identifyModule %d %s \t PACSetting = %d----------\n", module!=NULL, module->primitivePointer, PACSetting);
    char *type, *rep; //representation
    char *ns; //Noise sensitivity
    Node *param;

    //check for the most primitive module
    if(isPrimitive(module)) {
        type = getModuleName(module);   //module name
        param = handleInput(module->inputDefPointer);   //dc: send param to upper/parent modules for composition
        
        if(PACSetting==0) {
            rep = getRepresentation(type);  //incase of switch, go up a module and decifer the representation
            printf("in isPrimitve type = %s \t rep = %s\n", type, rep);
            return rep; 
        }
        else if(PACSetting==1) {
            ns = getNoiseSensitivity(type);  //incase of switch, go up a module and decifer the representation
            printf("in isPrimitve type = %s \t ns = %s\n", type, ns);
            return ns; 
        }
    }
    // check for upper modules - make a list of submodules
    PrimNode* subModuleList = NULL;
    findSubmodules(module, &subModuleList);
    int numberOfSubModules = 0;
    ModuleNode* subMod;
    printf("--------------------after findSubmodules--------------------\n");
    PrimNode* subModulesIter = subModuleList;

    // iterate through the submodules of the given module 
    while(subModulesIter) {
        printf("numberOfSubModules: %d    subModulesIter : %s\n", numberOfSubModules, subModulesIter->primitiveName);

        //the primitive module definition does not exists - it is either a primitive element or not defined
        if(!subModulesIter->modulePointer) { 
            printf("modulePointer not available - Submodule not defined as it is primitive\n");
            printf("subModulesIter->primitiveName = %s\n", subModulesIter->primitiveName);
            rep = getRepresentation(subModulesIter->primitiveName);
            ns = getNoiseSensitivity(subModulesIter->primitiveName);
            subModulesIter->rep = rep;
            subModulesIter->ns = ns;
            if(PACSetting==0) {
                printf("rep = %s\n", rep);
                return rep;
            }
            else if(PACSetting==1) {
                printf("ns = %s\n", ns);
                return ns;
            }
        }
        else {
            printf("modulePointer available!!\n");
            subMod = subModulesIter->modulePointer; //pointer to the submodule
            printf("in while ===> %s %d\n", subModulesIter->primitiveName, subMod!=NULL);
            printf("-----------------------------------------------------\n");
            printf("module = %s \t submodule = %s\n", module->primitivePointer, subMod->primitivePointer);
            printf("-----------------------------------------------------\n");
        // if(subModulesIter->visited==0) { // remove visited condition as the module might be visited twice if parent module is not learnable in the DI setting
            
            char* result = identifyModule(subMod, PACSetting);
            if(PACSetting==0) {
                subModulesIter->rep = result;
                subModulesIter->ns = NULL;
                printf("in DI PACSetting After identifyModule %s -> rep = %s\n", module->primitivePointer, subModulesIter->rep);
            }
            else if(PACSetting==1) {
                subModulesIter->ns = result;
                printf("in DD PACSetting After identifyModule %s -> ns = %s\n", module->primitivePointer, subModulesIter->ns);
            }
            subModulesIter->visited = 1;
        // }
        // else {
        //     printf("in else subModulesIter->rep  = %s -> %s\n", subModulesIter->rep, rep);
        // }
        }
        numberOfSubModules +=1;
        subModulesIter = subModulesIter->nextPrim;
    }

    printf("In %s, numberOfSubModules = %d\n", module->primitivePointer, numberOfSubModules);
    type = getModuleName(module);
    if(numberOfSubModules==1) {
        subModulesIter = subModuleList;
        subMod = subModulesIter->modulePointer;
        rep = subModulesIter->rep;
        ns = subModulesIter->ns;
        char *it = identifyInputTransformation(module, subMod);
        char *ot = identifyOutputTransformation(module, subMod);
        printf("-----------------------type = %s \t numberOfSubModules = 1 \t it = %s \t ot = %s\n", type, it, ot);
        if(PACSetting==0) {
            rep = composeRepresentation(rep, it, ot);
            printf("in DI Setting \t composed rep = %s\n", rep);
            return rep;    
        }
        else if(PACSetting==1) {
            ns = composeNoiseSensitivity(ns, rep, it, ot, param);
            printf("in DD Setting \t composed rep = %s\n", rep);
            return ns;
        }
    }
    //generalize it to >=2 submodules by considering 2 at a time 
    else if(numberOfSubModules==2) {
        printf("-----------------------type = %s \t numberOfSubModules = %d\t %d\n", type, numberOfSubModules, subModuleList!=NULL);
        subModulesIter = subModuleList;
        printf("subModulesIter = %s\n", subModulesIter->primitiveName);
        ModuleNode* currentSubmodule = subModulesIter->modulePointer;
        // if()
        ModuleNode* nextSubmodule = subModulesIter->nextPrim->modulePointer;
        char *relation = check_submodule_connection_2(module, currentSubmodule, nextSubmodule);
        char *rep1 = subModuleList->rep;            //representation of first submodule
        char *rep2 = subModuleList->nextPrim->rep;  //representation of second submodule
        printf("-- rep1 = %s \t rep2 = %s \t relation = %s \n", rep1, rep2, relation);
        // printf("-- rep1 = %s \t rep2 = %s \n", rep1, rep2); //, relation);
        type = getModuleName(module);
        rep = composeRepresentationGeneric(rep1, rep2, relation);
        printf("----> type = %s \t rep = %s\n", type, rep);
        if(PACSetting==0) {
            return rep;
        }
        else 
            return rep; // change to NS

        /*while(subModulesIter) {
            ModuleNode* currentSubmodule = subModulesIter->modulePointer;
            if(subModulesIter->nextPrim) {
                ModuleNode* nextSubmodule = subModulesIter->nextPrim->modulePointer;

                char *relation = check_submodule_connection_2(module, currentSubmodule, nextSubmodule);
                char *rep1 = subModuleList->rep;//representation of first submodule
                char *rep2 = subModuleList->nextPrim->rep;//representation of second submodule
                // printf("-- rep1 = %s \t rep2 = %s \t relation = %s \n", rep1, rep2, relation);
                printf("-- rep1 = %s \t rep2 = %s \n", rep1, rep2); //, relation);
                type = getModuleName(module);
                // rep = composeRepresentationGeneric(rep1, rep2, relation);
                printf("----> type = %s \t rep = %s\n", type, rep);
            }

            subModulesIter = subModulesIter->nextPrim;
        }*/
        // char *relation = check_submodule_connection(module, subModuleList);
        // char *rep1 = subModuleList->rep;//representation of first submodule
        // char *rep2 = subModuleList->nextPrim->rep;//representation of second submodule
        // printf("-- rep1 = %s \t rep2 = %s \t relation = %s \n", rep1, rep2, relation);
        // type = getModuleName(module);
        // rep = composeRepresentationGeneric(rep1, rep2, relation);
        // printf("----> type = %s \t rep = %s\n", type, rep);
        // return rep;
        // return "";
    }


}

Node* modifyParam (ModuleNode *module, char* rep, Node* param) {
    // printf("modifyParam\n");
    Node *modParam = param;

    if(strcmp(module->primitivePointer, "APUF")==0) {
        printf("in APUF\n");
        Node *var = modParam;

        //append m node at end
        Node *newnode;
        if((newnode = (Node*)malloc(sizeof(Node))) != NULL) {
            newnode->param = "m";
            newnode->coeff = 1;
            newnode->power = 1;
        }
        else {
            printf("Malloc Error: Cannot Create param node!\n");
        }
        while(var->next) {
            var = var->next;
        }
        var->next = newnode;
    }
    if(strcmp(module->primitivePointer, "DAPUF")==0) {
        //k = kC2;
        Node *var = modParam;
        //update k
        while(var) {
            if(strcmp(var->param,"k")==0) {
                var->power = 2; //asymptotic
            }
            var = var->next;
        }

        //append m node at end
        Node *newnode;
        if((newnode = (Node*)malloc(sizeof(Node))) != NULL) {
            newnode->param = "m";
            newnode->coeff = 1;
            newnode->power = 1;
        }
        else {
            printf("Malloc Error: Cannot Create param node!\n");
        }
        var=  modParam;
        while(var->next) {
            var = var->next;
        }
        var->next = newnode;
    }
    else if(strcmp(module->primitivePointer, "XORAPUF")==0) {
        //update n
        Node *var = modParam;
        char *k;
        while(var) {
            if(strcmp(var->param,"k")==0) {
                k = var->param;
                if(var->power != 1) {
                    char *power;
                    sprintf(power,"%d",var->power);
                    k = strcat(strcat(strcat(k, "^("),power),")");
                }
                if(var->coeff != 1) {
                    char *coeff;
                    sprintf(coeff,"%d",var->coeff);
                    k = strcat(strcat(coeff, "*"),k);
                }
                var->param = k;
            }
            var = var->next;
        }

        //append m node at end
        Node *newnode;
        if((newnode = (Node*)malloc(sizeof(Node))) != NULL) {
            newnode->param = "m";
            newnode->coeff = 1;
            newnode->power = 1;
        }
        else {
            printf("Malloc Error: Cannot Create param node!\n");
        }
        var=  modParam;
        while(var->next) {
            var = var->next;
        }
        var->next = newnode;
    }
    /*else if(strcmp(module->primitivePointer, "FFAPUF")==0) {
        Node *var = modParam;
        char *k;
        while(var) {
            if(strcmp(var->param,"k")==0) {
                k = var->param;
                if(var->power != 1) {
                    char *power;
                    sprintf(power,"%d",var->power);
                    k = strcat(strcat(strcat(k, "^("),power),")");
                }
                if(var->coeff != 1) {
                    char *coeff;
                    sprintf(coeff,"%d",var->coeff);
                    k = strcat(strcat(coeff, "*"),k);
                }
                var->param = k;
            }
            var = var->next;
        }

        //append m node at end
        Node *newnode;
        if((newnode = (Node*)malloc(sizeof(Node))) != NULL) {
            newnode->param = "m";
            newnode->coeff = 1;
            newnode->power = 1;
        }
        else {
            printf("Malloc Error: Cannot Create param node!\n");
        }
        var=  modParam;
        while(var->next) {
            var = var->next;
        }
        var->next = newnode;
    }*/
    return modParam;
}

int isConstant (char *var) {
    // printf("in isConstant %s \n", var);
    char* v = strtok(var, "^");
    char* w = strtok(NULL, "^");
    // printf("base = %s \t exp = %s \n", v, w);
    int is_const = 0;
    for(int i=0;i<strlen(v);i++) {
        if(v[i]>='0' && v[i]<='9')
            is_const = 1;
        else if(v[i]=='k')
            is_const = 1;
        else if(v[i]=='(' || v[i]==')' || v[i]=='{' || v[i]=='}' || v[i]=='[' || v[i]==']') {
            // printf("in else\n");
            continue;
        }
        // printf("%c\n", v[i]);
    }
    // printf("is_const = %d\n", is_const);
    return is_const;
}

int checkPoly (char *var) {
    //returns 1 if var is poly in n,m,k..parameters
    //else returns 0
    // printf("checkPoly \n");
    char* v = strtok(var, "^");
    char* w = strtok(NULL, "^");
    // printf("in checkPoly \n var = %s \n v = %s \n w = %s \n", var, v, w);
    if(!w || isConstant(w))
        return 1;
    else
        return 0;
}


void computeSampleComplexity (Node *param, int PACSetting, char *model, char* ns) {
    printf("computeSampleComplexity \t rep = %s \t PACSetting = %d \n", model, PACSetting);
    Node *var = param;
    Node *n = NULL, *m = NULL, *k = NULL, *ff_in = NULL, *ff_out = NULL, *s = NULL;//, *eta;
    while(var) {
        printf("in while var = %s\n", var->param);
        if(strcmp(var->param,"n")==0) {
            n = var;
            // printf("%s\n", n->param);
        }
        else if(strcmp(var->param,"k")==0) {
            k = var;
            // printf("%s\n", k->param);
        }
        else if(strcmp(var->param,"m")==0) {
            m = var;
            // printf("%s\n", m->param);
        }
        else if(strcmp(var->param,"ff_in")==0) {
            ff_in = var;
            // printf("%s\n", ff_in->param);
        }
        else if(strcmp(var->param,"ff_out")==0) {
            ff_out = var;
            // printf("%s\n", ff_out->param);
        }
        else if(strcmp(var->param,"s")==0) {
            s = var;
            // printf("%s\n", s->param);
        }
        else if(strcmp(var->param,"kl")==0) {//for interpose
            k = var;
            // printf("%s\n", s->param);
        }
        var = var->next;
    }
    // printf("after reading variables %s\n", n->param);
    if(PACSetting==0) {
        if(strcmp(model, "DFA")==0) {
            // printf("in dfa\n");
            //parameter to be checked = no of states
            //if no of states is poly then puf is PAC Learnable
            char n_states[100];
            strcpy(n_states, n->param);
            strcat(strcat(strcat(strcat(strcat(n_states, "*"), "("), "m"), "+1)"), "^2"); //format of sample complexity for dfa rep
            printf("n_states = %s\n", n_states);
            if(checkPoly(n_states)) {
                printf("---------------------------------------------------------------\nPUF is PAC Learnable using DFA Representation\n---------------------------------------------------------------\n\n");
            }
            else {
                
                printf("---------------------------------------------------------------\nPUF is not PAC Learnable under this representation\n---------------------------------------------------------------\n\n");
            }
        }
        else if(strcmp(model, "LTF")==0) {
            //parameter to be checked = mistake bound = R/e
            //if R(input size) is poly and 
            //e(epsilon with or without the noise coefficient) is poly then puf is PAC Learnable
            printf("In LTF\n");
            char *open = "(";
            char n_mis_num[100], n_mis_denom[100];
            strcpy(n_mis_num, open);
            // strcpy(n_mis_denom, open);
            strcat(strcat(strcat(strcat(n_mis_num, n->param), "+1)^"), k->param), "^2");

            strcat(n_mis_denom, "e)^2");
            // n_mis = len;
            printf("n_mis_num = %s\n", n_mis_num);
            if(checkPoly(n_mis_num)) {
                printf("---------------------------------------------------------------\nPUF is PAC Learnable using LTF Represenatation\n---------------------------------------------------------------\n\n");
            }
            else {
                
                printf("---------------------------------------------------------------\nPUF is not PAC Learnable under this representation\n---------------------------------------------------------------\n\n");
            }
        }
        else if(strcmp(model,"LTF_N")==0) {
            //parameter to be checked = mistake bound = R/e'
            //if R(input size) is poly and e' is poly
            //e'(epsilon with the noise coefficient) is poly then puf is PAC Learnable
            printf("in ltf_n else\n");
            char *open = "(";
            // strcpy(open,"(");
            char* len;
            char* bias;//[100];// = NULL;
            char* eq;//[100];//term within tan inv
            bias = (char*)malloc(sizeof(1000));
            eq = (char*)malloc(sizeof(1000));
            if(s!=NULL) {
                // printf("in interpose\n");
                strcat(eq, "(");
                strcat(strcat(strcat(strcat(strcat(strcat(strcat(eq, "(2*"), s->param), "-1)/(2*"), n->param), "+1 - 2*"), s->param), "))^(1/2)");
                // eq = strcat(strcat(strcat(strcat(strcat(eq,"(2*"), s->param), "-1)/(2*"), n->param), "+1 - 2*");//, s->param), "))^(1/2)");
                // eq = strcat(strcat(strcat(strcat(strcat(strcat(strcat(eq,"(2*"), s->param), "-1)/(2*"), n->param), "+1 - 2*"), s->param), "))^(1/2)");
                printf("eq = %s \t %s \n", eq, s->param);
            }
            else if(ff_in!=NULL && ff_out!=NULL) {
                // printf("in ff \n");
                // printf("eq = %s %s\n", eq, open);
                strcat(eq,"(");
                // eq = strcat(strcat(open,"(2*"), ff_out->param);
                strcat(strcat(strcat(strcat(strcat(strcat(strcat(eq,"(2*"), ff_out->param), " - 1)/(2*"), n->param), " + 1 - 2*"), ff_out->param), "))^(1/2)");
                printf("eq = %s \n", eq);
            }
            if(k==NULL) {
                // printf("in ffapuf k\n");
                strcpy(bias,"1-");
                // printf("bias = %s\n", bias);
                strcat(strcat(strcat(bias,"(2/pi)arctan("), eq ), ")");
                // printf("bias = %s\n", bias);
            }
            else {
                // printf("in k else bias = %s \n", bias);
                // bias=  NULL;
                // strcpy(bias,"(");
                bias = eq;
                // printf("eq = %s\n", eq);
                // bias = strcat(strcat(strcat(strcat(strcat(strcat(bias,"2^("),k->param),")(1/2-(2/pi)arctan"), eq ), ")^"), k->param);
                // printf("bias = %s\n", bias);
            }
            char* e;
            // char* n_mis;
            // strcat(strcat(len, "/"), e);
            printf("bias = %s \n", bias);
            if(checkPoly(bias)) {
                printf("---------------------------------------------------------------\nPUF is PAC Learnable using LTF with Noise Represenatation\n---------------------------------------------------------------\n\n");
            }
            else {
                
                printf("---------------------------------------------------------------\nPUF is not PAC Learnable under this representation\n---------------------------------------------------------------\n\n");
            }
        }
        else if(strcmp(model, "DL")==0) {
            //parameter to be checked = size of DL measured in bits
            //if size is poly then puf is PAC Learnable
            char *open;
            strcpy(open,"(");
            char *len = strcat(strcat(open, n->param), "-1)");
            // printf("len = %s \t open  %s n = %s \n", len, open, n->param);//, strcat(open, len));
            char *bits;
            bits = strcat(strcat(strcat(len, "log("), n->param), "-1)");
            // printf("bits = %s \t open = %s\n", bits, open);

            if(checkPoly(bits)) {
                printf("---------------------------------------------------------------\nPUF is PAC Learnable using Decision List Represenatation\n---------------------------------------------------------------\n\n");
            }
            else {
                
                printf("---------------------------------------------------------------\nPUF is not PAC Learnable under this representation\n---------------------------------------------------------------\n\n");
            }
        }
    }
    /*else if(PACSetting==1) {

    }*/
    else if(PACSetting==2) {  //done
        if(strcmp(model, "LTF")==0) {
            char* dimension = n->param;
            char* VC_dim;
            strcpy(VC_dim, dimension);
            strcpy(VC_dim, "+1)");
            printf("VC_Dim = %s\n", VC_dim);
            if(checkPoly(VC_dim)) {
                printf("---------------------------------------------------------------\nPUF is PAC Learnable using LTF Represenatation\n---------------------------------------------------------------\n\n");
            }
            else {
                printf("---------------------------------------------------------------\nPUF is not PAC Learnable under this representation\n---------------------------------------------------------------\n\n");
            }
        }
    }

}

/*void computeNoiseSensitivity (Node* param, char* model) {
    
}*/

// void computeSampleComplexityFromVCDimension (Node* param, char* model) {
// }

void extraxtPACComplexityFromPUFGObjectModel() {
    ModuleNode *currentModuleNode = headModuleNode;
    while(currentModuleNode) {
        printf("******%s\n", currentModuleNode->primitivePointer);
        currentModuleNode = currentModuleNode->nextModule;
    }
    currentModuleNode = headModuleNode;
    printf("\n---extraxtPACComplexityFromPUFGObjectModel %d---\n\n",(currentModuleNode!=NULL));
    if(currentModuleNode->primitivePointer==NULL) {
        printf("Incomplete Module Definition\n");
        fprintf(yyout,"// Module Error: Module (PrimitiveNode) is not Specified !!\n");
    }
    else if(currentModuleNode->statementPointer==NULL) {
        printf("Incomplete Module Definition\n");
        fprintf(yyout,"// Module Error: Module (StatementNode) is not Specified !!\n");
    }
    else if(currentModuleNode->inputDefPointer==NULL) {
        printf("Incomplete Module Definition\n");
        fprintf(yyout,"// Module Error: Module (InputDefNode) is not Specified !!\n");
    }
    char *primnode = currentModuleNode->primitivePointer;
    InputDefNode *inputdefnode = currentModuleNode->inputDefPointer;

    Node* param = handleInput(inputdefnode);
    printf("--------------------after handleInput--------------------\n");

    int PACSetting = 0; //PACSetting = 0 for distribution indpendent and 1 for uniform distribution and 2 for VC dimension
    char* rep = identifyModule(currentModuleNode, PACSetting);
    char* NS;
    printf("--------------------after identify_module-------------------- rep = %s \n", rep);
    if(rep=="-") {
        printf("Modifying PACSetting to 1\n");
        PACSetting = 1;
        NS = identifyModule(currentModuleNode, PACSetting); //replace currentModule with lastModule - last module where rep was not found
    }
    // param = modify_param(currentModuleNode, rep, param);
    // param = modifyParam(currentModuleNode, PACSetting, rep, NS); --check significance
    // compute_sample_complexity(param, rep);
    computeSampleComplexity(param, PACSetting, rep, NS);

}




/*****************************************************************
 * Main Function Definition Section
 *****************************************************************/
int main(int argc, char **argv){
    // unsigned long int start = timestamp();
    // printf("in main\n");
    // Opening the Read-Write Files From Command-Line Input
    if(argc > 3){
        fprintf(stderr,"Error: Too Many Arguements!!\n");
        printf("Error: Too Many Arguements!!\n");
        exit(0);
    }
    if(argc > 1){
        inputFile = argv[1];
        yyin = fopen(inputFile,"r");
        if(yyin == NULL){
            fprintf(stderr,"Error: Cannot Open Input File!!\n");
            printf("Error: Cannot Open Input File!!\n");
            exit(0);
        }
    }
    else {
        inputFile = "test.pufg";
        yyin = fopen(inputFile,"r");
        if(yyin == NULL){
            fprintf(stderr,"Error: Cannot Open Input File!!\n");
            printf("Error: Cannot Open Input File!!\n");
            exit(0);
        }
    }
    if(argc > 2){
        outputFile = argv[2];
    }
    else{
        outputFile = defaultOutputFile;
    }
    yyout = fopen(outputFile,"w");
    if(yyout == NULL){
        fprintf(stderr,"Error: Cannot Open Output File!!\n");
        printf("Error: Cannot Open Output File!!\n");
        exit(0);
    }
    // printf("before parsing %d\n", yyin!=NULL);
    // Parsing Routine & Forming Object Model from input PUF design
    yyparse();
    // printf("after parsing\n");
    // Extraction of PUF Properties from Object Model
    extraxtPACComplexityFromPUFGObjectModel();

    // Closing Read-Write Files
    fclose(yyin);
    fclose(yyout);
    // unsigned long int end = timestamp();
    // printf("time taken : %ld\n",end-start);
    return 0;
}