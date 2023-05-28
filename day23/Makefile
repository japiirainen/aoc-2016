INPUTS=sample.txt input.txt

build: main.ml
	dune build

watch:
	dune build --watch

run:
	./_build/default/main.exe $(INPUTS)
