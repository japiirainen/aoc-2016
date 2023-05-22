INPUTS=sample.txt input.txt

build: main.go
	go build main.go

run: build
	./main $(INPUTS)

clean:
	rm -fv main
