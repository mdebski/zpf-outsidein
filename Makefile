.PHONY: all clean

all:
	ghc --make Main

clean:
	rm -f *.o *.hi *.ho Main
