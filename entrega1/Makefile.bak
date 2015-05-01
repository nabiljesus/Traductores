DOCDIR   = doc
BINARIES = Lanscii
OBJECTS  = *.o *.hi
SOURCES  = Lanscii.hs
MODULES  = Lexer.x


all:	$(BINARIES) doc

Lanscii: $(SOURCES) $(MODULES)
	alex $(MODULES)
	ghc --make Lanscii
	
clean:
	rm -rf Lex*.hs $(OBJECTS) $(BINARIES) doc

doc:	$(SOURCES) $(MODULES)
	test -d $(DOCDIR) || mkdir $(DOCDIR)
	rm -rf $(DOCDIR)/*
	ghc -cpp -E -optP-P -D__HADDOCK__ Lexer.hs -o Lex.hs
	haddock --html --odir=$(DOCDIR) $(SOURCES) Lex.hs
	rm -f Lex.hs
