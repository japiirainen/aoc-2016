INPUTS=sample.txt

build: main.zig
	zig build-exe main.zig

run: build
	./main $(INPUTS)

clean:
	rm main *.o
