

all: build test

OPTS?=--profile=release
build:
	@dune build @install $(OPTS)

test:
	@dune runtest --no-buffer --force $(OPTS)

clean:
	@dune clean

doc:
	@dune build @doc

WATCH?= @install @runtest
watch:
	@dune build $(OPTS) $(WATCH) -w

.PHONY: benchs tests build watch

VERSION=$(shell awk '/^version:/ {print $$2}' tiny_httpd.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed --follow-symlinks -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/**.ml) $(wildcard src/**.mli) \
			$(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
	sed --follow-symlinks -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/**.ml) $(wildcard src/**.mli) \
		$(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
