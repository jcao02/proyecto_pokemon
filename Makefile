all: clean pokesim

pokesim: 
	ghc --make pokesim.hs 

clean:
	rm *.o *.hi pokesim || true
