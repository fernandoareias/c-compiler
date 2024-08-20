yacc -v -d parser5.y 
lex lexer.l 
gcc -ll y.tab.c 
./a.out < input1.c