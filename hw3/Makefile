.PHONY: pack all run clean

COMPILER=ghc
OPTIONS=--make -dynamic -O3 -i:src:lib -Ilib -outputdir out -o out/main
SOURCES=$(shell find src app lib -type f -name "*.hs")
PARSER=$(shell find gen -type f -name "*.[xy]")

all: out
	alex gen/Lexer.x
	happy gen/Parser.y
	mv gen/*.hs src/
	$(COMPILER) $(OPTIONS) app/Main.hs

run:
	./out/main

out:
	mkdir -p out

pack:
	zip bezborodov.zip -r Makefile $(SOURCES) $(PARSER)

clean:
	rm -rf out
