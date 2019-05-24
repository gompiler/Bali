COMPILE_FLAGS = --pedantic --coverage

.PHONY: all
all: bali

.PHONY: clean
clean:
	stack clean --full
	@rm -f bali.tix

.PHONY: build
build:
	stack build $(COMPILE_FLAGS)

.PHONY: report
report:
	stack hpc report bali.tix

bali: build
	@rm -f bali
	@rm -f bali.tix
	@ln -s .stack-work/*/*/*/*/bin/bali $@

# Continuous build, builds at every file modification, for development purposes only
.PHONY: cbuild
cbuild:
	stack build  --file-watch $(COMPILE_FLAGS)

# Generate test resources
.PHONY: gen
gen:
	git submodule update --init --recursive
	python3 "scripts/init_test_resources.py"

# Run hspec tests
.PHONY: test
test: gen
	@rm -f bali.tix
	stack test $(COMPILE_FLAGS)

.PHONY: ctest
ctest:
	@rm -f bali.tix
	stack test --file-watch $(COMPILE_FLAGS)

# Refactor code using hlint, hindent and stylish-haskell
.PHONY: refactor
refactor:
	@find . -name "*.hs" -not -path "./out/*" -not -path "./.stack-work/*" -exec hlint --refactor --refactor-options="--inplace" {} \; -exec hindent {} \; -exec stylish-haskell -i {} \;
