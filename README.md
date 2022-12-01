# PARLE-G: <ins>P</ins>rovable <ins>A</ins>utomated <ins>R</ins>epresentation and Analysis Framework for <ins>L</ins>earnability <ins>E</ins>valuation of <ins>G</ins>eneric PUF Composition
PARLE-G is an automated learnability assessment framework that evaluates the provable learnability of PUF constructions and compositions in the Probably Approximately Correct (PAC) Learning model. The tool takes a PUF design as input, represented in PUF-G language and outputs a sample complexity of the learning algorithm corresponding to the chosen representation class. The reprsentation class is chosen dependent on the analytical model of the input construction. 

## PUF-G Grammar
We propose a formal representation language to uniformly represent PUF construction and compositions. The grammar supporta a set of primitives used to define basic building blocks and a set of operations. The simplified syntactical grammar of the PUF-G language is given as follows:

> TOP::               MODULE TOP 
                    | MODULE
                    
> MODULE::            begin PRIMITIVE ( INPUT_DEF )
                        STATEMENTS
                        OUTPUT_DEF
                    end PRIMITIVE
                    
> PRIMITIVE::         PUF_PRIMITIVE | BASIC_PRIMITIVE

> PUF_PRIMITIVE::     APUF | ROPUF 

> BASIC_PRIMITIVE::   ARBITER | MUX_2x1 | SWITCH_2x2 | DELAY-CHAIN
                  | FF-DELAY-CHAIN | RING_OSC 

> INPUT_DEF::         DATA_TYPE TUPLE DELIMITER INPUT_DEF
                  | DATA_TYPE TUPLE | //no-input

> OUTPUT_DEF::        return ( INPUT_DEF ) DELIMITER | //no-output

> PRIMITIVE_CALL::    PRIMITIVE ( VARIABLES )

> TUPLE:              < VARIABLES > | STRING | NUMBER

> VARIABLES::         TUPLE DELIMITER VARIABLES | TUPLE | //null

> STRING::            [a-zA-Z_][a-zA-Z0-9_]*

> NUMBER::            [1-9][0-9]*

> DELIMITER::         ; | ,  //semi-colon or comma

> STATEMENTS::        STATEMENT STATEMENTS | STATEMENT

> STATEMENT::         ASSIGNMENT | IFELSE_STATEMENT
                  | SERIAL_STATEMENT | PARALLEL_STATEMENT

> ASSIGNMENT::        STRING = EXPRESSION DELIMITER
                  | TUPLE = PRIMITIVE_CALL DELIMITER
                  | DELIMITER    //null-statement

> EXPRESSION::        EXPRESSION ARITHMETIC_OPERATOR EXPRESSION
                  | EXPRESSION LOGICAL_OPERATOR EXPRESSION
                  | ( EXPRESSION ) | not EXPRESSION | STRING | NUMBER

> ARITHMETIC_OPERATOR::/ | * | + | - | %

> LOGICAL_OPERATOR::  and | or | xor | == | != | <= | >= | < | >

> IFELSE_STATEMENT::  if EXPRESSION then STATEMENTS end if
                  | if EXPRESSION then STATEMENTS end if
                    else STATEMENTS end if
                  | if EXPRESSION then STATEMENTS end if
                    ELSEIF_STATEMENTS
                    else STATEMENTS end if

> ELSEIF_STATEMENTS:: else if EXPRESSION then STATEMENTS end if
                    ELSEIF_STATEMENTS
                  | else if EXPRESSION then STATEMENTS end if

> SERIAL_STATEMENT::  serial ASSIGNMENT to EXPRESSION do
                        STATEMENTS
                    end serial

> PARALLEL_STATEMENT::parallel ASSIGNMENT to EXPRESSION do
                        STATEMENTS
                    end parallel

Refer to the link [here](http://cse.iitkgp.ac.in/~debdeep/osscrypto/PUFG.pdf) for the complete grammar and example representations. 

## Prerequisites

### For Linux (debian based systems)
1. Install Lex and YACC packages
> sudo apt-get install flex
> sudo apt-get install bison

2. Install GCC Compiler
> sudo apt install build-essential


## Steps to run

1. Compile Lex File
> lex puf_g.l
2. Compile YACC file
> yacc -d puf_g.y
3. Compile the generated C files
> cc -c lex.yy.c y.tab.c
4.  Create Object files
> cc -o puf_g.out lex.yy.o y.tab.o -ll
5. Execute the object file with the input PUF description
> ./puf_g.out <file_name>
