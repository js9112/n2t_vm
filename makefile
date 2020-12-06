assembler:
	mkdir -p bin
	ghc Main.hs -o bin/VMTranslator -hidir bin -odir bin

clean:
	rm -rf bin/*
