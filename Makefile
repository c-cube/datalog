
all: build test bottom_up top_down

build:
	@dune build @install

bottom_up:
	@dune build src/bottom_up_cli/datalog_cli.exe
	@ln -sf _build/default/src/bottom_up_cli/datalog_cli.exe

top_down:
	@dune build src/top_down_cli/topDownCli.exe
	@ln -sf _build/default/src/top_down_cli/topDownCli.exe

test: build
	@dune runtest --no-buffer --force

clean:
	@dune clean

format:
	@dune fmt --auto-promote

format-check:
	@dune fmt --quiet

doc:
	@dune build @doc

VERSION=$(shell awk '/^version:/ {print $$2}' datalog.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" src/*.ml src/*.mli
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" src/*.ml src/*.mli

WATCH?=@all
watch:
	@dune build $(TO_WATCH) -w

.PHONY: benchs tests update_next_tag watch

