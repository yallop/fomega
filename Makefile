#
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable file fomega
#   make windows to rebuild the executable file fomega.exe
#   make test    to rebuild the executable and run it on input file test.f
#   make clean   to remove all intermediate and temporary files
#   make depend  to rebuild the intermodule dependency graph that is used
#                  by make to determine which order to schedule
#	           compilations.  You should not need to do this unless
#                  you add new modules or new dependencies between
#                  existing modules.  (The graph is stored in the file
#                  .depend)

YACC = menhir --explain --strict --graph

# These are the object files needed to rebuild the main executable file
#
OBJS = support.cmo syntax.cmo core.cmo parser.cmo lexer.cmo process.cmo

# Files that need to be generated from other files
DEPEND += lexer.ml parser.ml

# Flags for compilation and linking
COMPILEFLAGS = -g
LINKFLAGS = -g
JSFLAGS =

# When "make" is invoked with no arguments, we build an executable
# typechecker, after building everything that it depends on
all: $(DEPEND) $(OBJS) fomega

js: fomega.js

# On a Windows machine, we do exactly the same except that the executable
# file that gets built needs to have the extension ".exe"
windows: $(DEPEND) $(OBJS) fomega.exe

# Include an automatically generated list of dependencies between source files
include .depend

# Build an executable typechecker
fomega: $(OBJS) main.cmo
	@echo Linking $@
	ocamlc $(LINKFLAGS) -o $@ $^

# Build an executable typechecker for Windows
fomega.exe: $(OBJS) main.cmo
	@echo Linking $@
	ocamlc $(LINKFLAGS) -o $@ $^

# Compile jsmain.ml
jsmain : $(OBJS) jsmain.ml
	@echo Linking $@
	ocamlfind ocamlc $(COMPILEFLAGS) -package js_of_ocaml -linkpkg -o $@ $^

fomega.js: jsmain
	@echo Compiling to JavaScript $@
	js_of_ocaml $(JSFLAGS) -o $@ $<

# Build and test
test: all
	./fomega test.f

# Compile an ML module interface
%.cmi : %.mli
	ocamlc $(COMPILEFLAGS) -c $<

# Compile an ML module implementation
%.cmo : %.ml
	ocamlc $(COMPILEFLAGS) -c $<

# Generate ML files from a parser definition file
parser.ml parser.mli: parser.mly
	@rm -f parser.ml parser.mli
	$(YACC) -v parser.mly
	@chmod -w parser.ml parser.mli

# Generate ML files from a lexer definition file
%.ml %.mli: %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@

# Clean up the directory
clean::
	rm -rf lexer.ml parser.ml parser.mli *.o *.cmo *.cmi parser.output \
           parser.automaton parser.conflicts parser.dot \
	   fomega fomega.exe TAGS *~ *.bak jsmain fomega.js

# Rebuild intermodule dependencies
depend:: $(DEPEND)
	ocamldep $(INCLUDE) *.mli *.ml > .depend

#
