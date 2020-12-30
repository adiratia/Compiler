lex project.l
yacc project.y
cc -o project y.tab.c -ll -Ly
./project < input_code.txt