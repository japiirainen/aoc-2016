INPUTS=sample.txt input.txt

Main.class: Main.java
	javac -Xlint:unchecked Main.java

run: Main.class
	java Main $(INPUTS)

clean:
	rm *.class
