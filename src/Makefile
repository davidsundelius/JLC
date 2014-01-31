all: libs
	bnfc Javalette.cf
	happy -gca ParJavalette.y
	alex -g LexJavalette.x
	ghc --make jlc.hs -o ../jlc

libs:
	javac ../src/Runtime.java -d ../lib
	llvm-as ../src/Runtime.ll -o ../lib/runtime.bc

clean:
	-rm -f *.log *.aux *.hi *.o *.dvi *.class *.j *.bak *.tex *.txt *.x *.y *~ TestJavalette.* SkelJavalette.*

distclean: clean
	-rm -f DocJavalette.* LexJavalette.* ParJavalette.* LayoutJavalette.* PrintJavalette.* AbsJavalette.* ErrM.* SharedString.* Javalette.dtd XMLJavalette.*

