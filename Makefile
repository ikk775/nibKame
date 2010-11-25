# see: http://www.ocaml.info/home/ocaml_sources.html#toc16

# put here the names of your source files (in the right order)
SOURCES = myUtil.ml id.ml sexpr.ml type.ml syntax.ml sread.ml typingType.ml typingExpr.ml typing.ml kNormal.ml

# the name of the resulting executable
RESULT  = executable.exe

# generate type information (.annot files)
ANNOTATE = yes


# make target (see manual) : byte-code, debug-code, native-code, ...
all: debug-code

include OCamlMakefile