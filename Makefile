
all: build test-dune

dev: build test

build:
	@dune build --profile=release @all

smtlib-cat:
	@dune build src/bin/smtlib_cat.exe --profile=release
	@ln -sf _build/default/src/bin/smt_cat.exe

test: test-dune test-cat test-idempotent

test-dune:
	@dune runtest --force --no-buffer

doc:
	@dune build @doc

clean:
	@dune clean

watch:
	@dune build @all -w

BENCH_DIR ?= benchmarks/

test-cat: build smtlib-cat
	@echo testing that '`parser`' works…
	@[ -d $(BENCH_DIR) ] || (echo "expect benchmarks/ to exist" && exit 1)
	@find $(BENCH_DIR) -name  '*.smt2' -print0 | xargs -0 ./smtlib_cat.exe -q

test-idempotent: build smtlib-cat
	@echo testing that '`printer | parser`' works…
	@find benchmarks -name  '*.smt2' -print | while read i ; do \
	  (./smtlib_cat.exe "$$i" | ./smtlib_cat.exe -q) || exit 1; \
	  done

.PHONY: doc watch clean test all build
