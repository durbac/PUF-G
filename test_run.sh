# echo "---------------------------------------------------------------" 
# echo "                           APUF" 
# echo "---------------------------------------------------------------" 
# lex puf_g.l
# yacc -d puf_g.y
# cc -c lex.yy.c y.tab.c
# cc -o puf_g.out lex.yy.o y.tab.o -ll
#  ./puf_g.out puf_designs/test_apuf.pufg 

# echo "---------------------------------------------------------------"
# echo "                           XORAPUF"
# echo "---------------------------------------------------------------" 
# lex puf_g.l
# yacc -d puf_g.y
# cc -c lex.yy.c y.tab.c
# cc -o puf_g.out lex.yy.o y.tab.o -ll
#  ./puf_g.out puf_designs/test_xorapuf.pufg 

# echo "---------------------------------------------------------------" 
# echo "                           ROPUF"
# echo "---------------------------------------------------------------" 
# lex puf_g.l
# yacc -d puf_g.y
# cc -c lex.yy.c y.tab.c
# cc -o puf_g.out lex.yy.o y.tab.o -ll
#  ./puf_g.out puf_designs/test_ropuf.pufg 

# echo "---------------------------------------------------------------" 
# echo "                           IPUF"
# echo "---------------------------------------------------------------" 
# lex puf_g.l
# yacc -d puf_g.y
# cc -c lex.yy.c y.tab.c
# cc -o puf_g.out lex.yy.o y.tab.o -ll
#  ./puf_g.out puf_designs/test_ipuf.pufg 

# echo "---------------------------------------------------------------"
# echo "                           DOMINO-IPUF"
# echo "---------------------------------------------------------------" 
# lex puf_g.l
# yacc -d puf_g.y
# cc -c lex.yy.c y.tab.c
# cc -o puf_g.out lex.yy.o y.tab.o -ll
#  ./puf_g.out puf_designs/test_domino_ipuf.pufg 

# echo "---------------------------------------------------------------"
# echo "                           XOR-DOMINO-IPUF"
# echo "---------------------------------------------------------------" 
# lex puf_g.l
# yacc -d puf_g.y
# cc -c lex.yy.c y.tab.c
# cc -o puf_g.out lex.yy.o y.tab.o -ll
#  ./puf_g.out puf_designs/test_xor_domino_ipuf.pufg 

# echo "---------------------------------------------------------------" 
# echo "                           XOR-IPUF"
# echo "---------------------------------------------------------------" 
# lex puf_g.l
# yacc -d puf_g.y
# cc -c lex.yy.c y.tab.c
# cc -o puf_g.out lex.yy.o y.tab.o -ll
#  ./puf_g.out puf_designs/test_xor_ipuf.pufg 

# echo "---------------------------------------------------------------" 
# echo "                           XOR-CASCADED-IPUF"
# echo "---------------------------------------------------------------" 
# lex puf_g.l
# yacc -d puf_g.y
# cc -c lex.yy.c y.tab.c
# cc -o puf_g.out lex.yy.o y.tab.o -ll
#  ./puf_g.out puf_designs/test_xor_cascaded_ipuf.pufg 

# echo "---------------------------------------------------------------" 
# echo "                           TREE-IPUF"
# echo "---------------------------------------------------------------" 
# lex puf_g.l
# yacc -d puf_g.y
# cc -c lex.yy.c y.tab.c
# cc -o puf_g.out lex.yy.o y.tab.o -ll
#  ./puf_g.out puf_designs/test_tree_ipuf.pufg 

echo "---------------------------------------------------------------" 
echo "                           MPUF"
echo "---------------------------------------------------------------" 
lex puf_g.l
yacc -d puf_g.y
cc -c lex.yy.c y.tab.c
cc -o puf_g.out lex.yy.o y.tab.o -ll
 ./puf_g.out puf_designs/test_mpuf.pufg 

# echo "---------------------------------------------------------------" 
# echo "                           REC-APUF"
# echo "---------------------------------------------------------------" 
# lex puf_g.l
# yacc -d puf_g.y
# cc -c lex.yy.c y.tab.c
# cc -o puf_g.out lex.yy.o y.tab.o -ll
#  ./puf_g.out puf_designs/test_rec-apuf.pufg 

# echo "---------------------------------------------------------------" 
# echo "                           REC-DAPUF"
# echo "---------------------------------------------------------------" 
# lex puf_g.l
# yacc -d puf_g.y
# cc -c lex.yy.c y.tab.c
# cc -o puf_g.out lex.yy.o y.tab.o -ll
#  ./puf_g.out puf_designs/test_rec-dapuf.pufg 