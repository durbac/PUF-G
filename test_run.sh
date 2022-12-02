echo "---------------------------------------------------------------" 
echo "                           APUF" 
echo "---------------------------------------------------------------" 
lex puf_g.l
yacc -d puf_g.y
cc -c lex.yy.c y.tab.c
cc -o puf_g.out lex.yy.o y.tab.o -ll
 ./puf_g.out puf_designs/test_apuf.pufg 

echo "---------------------------------------------------------------" # 
echo "                           XORAPUF"
echo "---------------------------------------------------------------" 
lex puf_g.l
yacc -d puf_g.y #puf_g_flatten_jan_backup.y
cc -c lex.yy.c y.tab.c
cc -o puf_g.out lex.yy.o y.tab.o -ll
 ./puf_g.out puf_designs/test_xorapuf.pufg 

echo "---------------------------------------------------------------" 
echo "                           ROPUF"
echo "---------------------------------------------------------------" 
lex puf_g.l
yacc -d puf_g.y
cc -c lex.yy.c y.tab.c
cc -o puf_g.out lex.yy.o y.tab.o -ll
 ./puf_g.out puf_designs/test_ropuf.pufg 