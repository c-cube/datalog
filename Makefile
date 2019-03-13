
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

doc:
	@dune build @doc

VERSION=$(shell awk '/^version:/ {print $$2}' datalog.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" src/*.ml src/*.mli
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" src/*.ml src/*.mli

TO_WATCH ?= all
watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		sleep 0.2; \
		make $(TO_WATCH); \
	done

.PHONY: benchs tests update_next_tag watch

