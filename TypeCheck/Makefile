all:
	bnfc Javalette.cf
	rm TestJavalette.hs
	happy -gca ParJavalette.y
	alex -g LexJavalette.x
	ghc --make Jlc.hs -o jlc

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocJavalette.ps
distclean: clean
	-rm -f *.bak DocJavalette.* LexJavalette.* ParJavalette.* LayoutJavalette.* SkelJavalette.* PrintJavalette.* TypeCheck AbsJavalette.* TestJavalette ErrM.* SharedString.* Javalette.dtd XMLJavalette.*
