# see: http://www.ocaml.info/home/ocaml_sources.html#toc16

# put here the names of your source files (in the right order)
SOURCES = 
SOURCES +=myUtilx.ml
SOURCES +=myUtil.ml
SOURCES +=sexpr.ml
SOURCES +=idx.ml
SOURCES +=id.ml
SOURCES +=debug.ml
SOURCES +=mangle.ml
SOURCES +=type.ml
SOURCES +=typingType.ml
SOURCES +=syntax.ml
SOURCES +=variant.ml
SOURCES +=sread.ml
SOURCES +=typingExpr.ml
SOURCES +=typing.ml
SOURCES +=module.ml
SOURCES +=pattern.ml
SOURCES +=kNormal.ml
SOURCES +=closure.ml
SOURCES +=virtualAsm.ml
SOURCES +=escapeAnalysis.ml
SOURCES +=basicblock.ml
SOURCES +=asmx86.ml
SOURCES +=instantiate.ml
SOURCES +=translationUnit.ml
SOURCES +=predefined.ml
SOURCES +=alpha.ml
SOURCES +=asmx86.ml
SOURCES +=main.ml

# the name of the resulting executable
RESULT  = nibkamec.exe

# generate type information (.annot files)
ANNOTATE = yes

LDFLAGS+=-static
OCAMLFLAGS= -g

# make target (see manual) : byte-code, debug-code, native-code, ...
all: debug-code

include OCamlMakefile
