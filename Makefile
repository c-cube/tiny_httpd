

all: build test

build:
	@dune build @install

test: tests/foo_50
	@dune runtest --no-buffer --force

tests/foo_50:
	dd if=/dev/zero of=$@ bs=1M count=50

clean:
	@dune clean

doc:
	@dune build @doc

watch:
	@dune build @all -w

.PHONY: benchs tests build watch
