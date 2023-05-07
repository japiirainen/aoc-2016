INPUTS=sample.txt input.txt

run: build
	./Main $(INPUTS)

build:
	agda -c Main.agda --ghc-flag="-o" --ghc-flag="Main" --compile-dir=_build

clean:
	rm -rf _build
