all:
	bnfc Javalette.cf
	rm TestJavalette.hs
	happy -gca ParJavalette.y
	alex -g LexJavalette.x
	ghc --make TypeCheck.hs -o TypeCheck
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocJavalette.ps
distclean: clean
	-rm -f *.bak DocJavalette.* LexJavalette.* ParJavalette.* LayoutJavalette.* SkelJavalette.* PrintJavalette.* TypeCheck.o TypeCheck.hi TypeCheck AbsJavalette.* TestJavalette ErrM.* SharedString.* Javalette.dtd XMLJavalette.*
