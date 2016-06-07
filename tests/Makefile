TESTS_HS=$(wildcard gadt_test*.hs)
TESTS=$(TESTS_HS:.hs=.e)

all: $(TESTS)
	echo $(TESTS)
	echo "TODO"

%.e: %.hs
	ghc -o $@ $< -i..

clean:
	rm -f *.e *.hi *.ho *.o
