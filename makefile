CC=ghc
CFLAGS=-O

OBJ = Main.o SpellingCorrector.o Trie.o
INTER = Main.hi SpellingCorrector.hi Trie.hi

.PHONY: all

all: build clean

build: 
	$(CC) $(CFLAGS) -o spelling-corrector Main.hs

run: build
	./spelling-corrector

clean:
	rm -f $(OBJ) $(INTER)

