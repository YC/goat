goat: src/*.rs libgoat_core.rlib
	rustc -O -o goat src/main.rs --extern goat_core=$$(pwd)/libgoat_core.rlib

libgoat_core.rlib: goat_core/src/*.rs
	rustc -O --crate-name goat_core --crate-type lib goat_core/src/lib.rs

clean:
	rm -f goat *.ll *.o *.rlib
