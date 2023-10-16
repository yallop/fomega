default: build

build:
	dune build @install

all:
	dune build @all

install:
	dune install

uninstall:
	dune uninstall

test:
	dune runtest

clean:
	dune clean

web:
	dune exec src/runweb.exe
