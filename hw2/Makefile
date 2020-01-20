.PHONY: pack all run clean

COMPILER=ghc
OPTIONS=--make -dynamic -O3 -i:src:lib -Ilib -outputdir out -o out/main
SOURCES=$(shell find src app lib -type f -name "*.hs")
GEN=$(shell find gen -type f)

all: out generate
	$(COMPILER) $(OPTIONS) app/Main.hs

run:
	./out/main

out:
	mkdir -p out

generate:
	alex gen/Lexer.x
	happy --strict gen/Parser.y
	mv gen/*.hs src

pack:
	zip bezborodov.zip -r Makefile $(SOURCES) $(GEN)

clean:
	rm -rf out
