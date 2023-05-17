INPUTS=input.txt

build: main.mlb main.sml
	mlton main.mlb

run: build
	./main $(INPUTS)

clean:
	rm main
