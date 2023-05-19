INPUTS=sample.txt input.txt

run: main.jar $(INPTUS)
	java -jar main.jar $(INPUTS)

main.jar: main.kt
	kotlinc main.kt -include-runtime -d main.jar
