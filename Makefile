.PHONY: all clean

all:
	ghc --make Main -prof -fprof-auto -fprof-cafs

clean:
	rm -f *.o *.hi *.ho Main
