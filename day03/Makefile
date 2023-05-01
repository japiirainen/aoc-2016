INPUTS=sample.txt input.txt
CC=clang

build: main.c
	$(CC) main.c -std=c17 -Wall -pedantic -o main

run: build
	./main $(INPUTS)
