COMPILE_FLAGS = --pedantic --coverage

.PHONY: all
all: bali

.PHONY: clean
clean:
	stack clean --allow-different-user --full
	@rm -f bali.tix

.PHONY: build
build:
	stack --allow-different-user build $(COMPILE_FLAGS)

.PHONY: report
report:
	stack --allow-different-user hpc report bali.tix

bali: build
	@rm -f bali
	@rm -f bali.tix
	@ln -s .stack-work/*/*/*/*/bin/bali $@

# Continuous build, builds at every file modification, for development purposes only
.PHONY: cbuild
cbuild:
	stack build  --allow-different-user --file-watch $(COMPILE_FLAGS)

# Run hspec tests
.PHONY: test
test: gen
	@rm -f bali.tix
	stack test --allow-different-user $(COMPILE_FLAGS)

.PHONY: ctest
ctest: gen
	@rm -f bali.tix
	stack test --allow-different-user --file-watch $(COMPILE_FLAGS)

# Refactor code using hlint, hindent and stylish-haskell
.PHONY: refactor
refactor:
	@find . -name "*.hs" -not -path "./out/*" -not -path "./.stack-work/*" -exec hlint --refactor --refactor-options="--inplace" {} \; -exec hindent {} \; -exec stylish-haskell -i {} \;