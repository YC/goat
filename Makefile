goat: src/*.rs
	rustc -O -o goat src/main.rs

clean:
	rm -f goat *.ll *.o
