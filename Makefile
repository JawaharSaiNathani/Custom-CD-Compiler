compiler: parser.y scanner.l
	yacc -d parser.y
	flex scanner.l
	gcc -o compiler lex.yy.c y.tab.h y.tab.c -ll -ly

clean:
	rm -f compiler lex.yy.c out.o out output.asm y.tab.c y.tab.h

assembly:
	nasm -f elf64 -o out.o output.asm
	ld out.o -o out
	./out

arithm:
	./compiler TestCases/Executable/arithm.cd

as:
	./compiler TestCases/Executable/arrays_sum.cd

dowhile:
	./compiler TestCases/Executable/dowhile.cd

function:
	./compiler TestCases/Executable/functions.cd

ifelse:
	./compiler TestCases/Executable/ifelse.cd

sortingarray:
	./compiler TestCases/Executable/sorting_arrays.cd

earrays:
	./compiler TestCases/Errors/arrays.cd

error2:
	./compiler TestCases/Errors/functions.cd

eprint:
	./compiler TestCases/Errors/print.cd

escan:
	./compiler TestCases/Errors/scan.cd

error1:
	./compiler TestCases/Errors/variabledeclarations.cd