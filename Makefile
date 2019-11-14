

all: build test

build:
	@dune build @install

test:
	@dune runtest --no-buffer --force

clean:
	@dune clean

doc:
	@dune build @doc

watch:
	@dune build @all -w

.PHONY: benchs tests build watch
