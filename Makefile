# see: http://www.ocaml.info/home/ocaml_sources.html#toc16

# put here the names of your source files (in the right order)
SOURCES = 
SOURCES +=myUtil.ml
SOURCES +=id.ml
SOURCES +=sexpr.ml
SOURCES +=debug.ml
SOURCES +=type.ml
SOURCES +=variant.ml
SOURCES +=syntax.ml
SOURCES +=sread.ml
SOURCES +=typingType.ml
SOURCES +=typingExpr.ml
SOURCES +=typing.ml
SOURCES +=kNormal.ml
SOURCES +=module.ml

# the name of the resulting executable
RESULT  = executable.exe

# generate type information (.annot files)
ANNOTATE = yes


# make target (see manual) : byte-code, debug-code, native-code, ...
all: debug-code

include OCamlMakefile
