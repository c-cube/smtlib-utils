
all: build test-dune

build:
	@dune build

tip-cat:
	@dune build src/bin/tip_cat.exe
	@ln -sf _build/default/src/bin/tip_cat.exe

test: test-dune test-cat test-idempotent

test-dune:
	@dune runtest --force --no-buffer

doc:
	@dune build @doc

clean:
	@dune clean

watch:
	while find src/ -print0 | xargs -0 inotifywait -e delete_self -e modify ; do \
		echo "============ at `date` ==========" ; \
		sleep 0.2; \
		make ; \
	done

test-cat: build tip-cat
	@git submodule update --init
	@[ -d benchmarks ] || (echo "expect benchmarks/ to exist" && exit 1)
	@find benchmarks/ -name  '*.smt2' -print0 | xargs -0 ./tip_cat.exe -q
	#find benchmarks-zipper/ -name  '*.smt2' -print0 | xargs -0 ./tip_cat.native -q

test-idempotent: build tip-cat
	@echo test that '`printer | parser`' works
	@for i in benchmarks/**/*.smt2 ; \
	  do echo "$$i"; \
	  (./tip_cat.exe "$$i" | ./tip_cat -q) || exit 1; \
	  done

.PHONY: doc watch clean test all build
