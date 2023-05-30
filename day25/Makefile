INPUTS=input.txt

build: Main.hs
	cabal build

run: Main.hs
	cabal run main $(INPUTS)

clean:
	rm -frd dist-newstyle

watch: Main.hs
	ghcid
