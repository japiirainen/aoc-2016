INPUTS=sample.txt input.txt

build: main.rs
	rustc main.rs

run: build
	./main $(INPUTS)

clean:
	rm main
