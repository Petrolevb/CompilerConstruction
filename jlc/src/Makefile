LIB=../lib/

all:
	bnfc Javalette.cf
	rm TestJavalette.hs
	happy -gca ParJavalette.y
	alex -g LexJavalette.x
	ghc --make Jlc.hs -o $(LIB)jlc
	javac $(LIB)Runtime.java
	llvm-as $(LIB)runtime.ll

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.bak
	-rm -f DocJavalette.ps
distclean: clean
	-rm -f *.bak DocJavalette.* LexJavalette.* ParJavalette.* LayoutJavalette.* SkelJavalette.* PrintJavalette.* TypeCheck AbsJavalette.* TestJavalette ErrM.* SharedString.* Javalette.dtd XMLJavalette.*
	-rm -f $(LIB)jlc $(LIB)*.class $(LIB)*.bc
