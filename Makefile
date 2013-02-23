.PHONY: all clean
all: ; cabal install --bindir .
clean: 
	rm pokesim
	rm -rf dist
