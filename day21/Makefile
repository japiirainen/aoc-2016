INPUTS=input.txt
DENO_FLAGS=--check --allow-read

run: main.ts
	deno run $(DENO_FLAGS) main.ts $(INPUTS)

watch: main.ts
	deno run $(DENO_FLAGS) --watch main.ts $(INPUTS)
