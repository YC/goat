goat:
	rustc -o goat src/main.rs

clean:
	rm -f goat *.ll *.o
