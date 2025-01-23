all: release

release:
	cargo build --release

debug:
	cargo build

test: release cargo-test python-test

cargo-test:
	cargo test

python-test: compile-sources
	cd tests && gdb -q -x run_tests.py && cd -

compile-sources:
	cd tests && bash compile_sources.sh && cd -


