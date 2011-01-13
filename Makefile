# see: http://www.ocaml.info/home/ocaml_sources.html#toc16

# put here the names of your source files (in the right order)
SOURCES = 
SOURCES +=myUtilx.ml
SOURCES +=myUtil.ml
SOURCES +=sexpr.ml
SOURCES +=idx.ml
SOURCES +=id.ml
SOURCES +=mangle.ml
SOURCES +=debug.ml
SOURCES +=type.ml
SOURCES +=variant.ml
SOURCES +=syntax.ml
SOURCES +=sread.ml
SOURCES +=typingType.ml
SOURCES +=typingExpr.ml
SOURCES +=typing.ml
SOURCES +=kNormal.ml
SOURCES +=closure.ml
SOURCES +=virtualAsm.ml
SOURCES +=module.ml
SOURCES +=instantiate.ml

# the name of the resulting executable
RESULT  = executable.exe

# generate type information (.annot files)
ANNOTATE = yes

OCAMLFLAGS= -g

# make target (see manual) : byte-code, debug-code, native-code, ...
all: debug-code

include OCamlMakefile
