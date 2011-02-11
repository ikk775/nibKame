# see: http://www.ocaml.info/home/ocaml_sources.html#toc16

# put here the names of your source files (in the right order)
SOURCES = 
SOURCES +=myUtilx.ml myUtilx.mli
SOURCES +=myUtil.ml
SOURCES +=sexpr.ml sexpr.mli
SOURCES +=idx.ml idx.mli
SOURCES +=id.ml
SOURCES +=debug.ml debug.mli
SOURCES +=mangle.ml mangle.mli
SOURCES +=type.ml type.mli
SOURCES +=typingType.ml typingType.mli
SOURCES +=syntax.ml syntax.mli
SOURCES +=variant.ml variant.mli
SOURCES +=sread.ml sread.mli
SOURCES +=typingExpr.ml typingExpr.mli
SOURCES +=typing.ml typing.mli
SOURCES +=lLifting.ml
SOURCES +=module.ml
SOURCES +=pattern.ml pattern.mli
SOURCES +=kNormal.ml kNormal.mli
SOURCES +=closure.ml closure.mli
SOURCES +=virtualAsm.ml virtualAsm.mli
SOURCES +=basicblock.ml basicblock.mli
SOURCES +=asmx86.ml
SOURCES +=instantiate.ml instantiate.mli
SOURCES +=translationUnit.ml translationUnit.mli
SOURCES +=predefined.ml
SOURCES +=main.ml

# the name of the resulting executable
RESULT  = nibkamec

# generate type information (.annot files)
ANNOTATE = yes

LDFLAGS+=-static
OCAMLFLAGS= -g

# make target (see manual) : byte-code, debug-code, native-code, ...
all: debug-code

LATEX:=platex
DVIPDF:=dvipdfmx
ifdef COMSPEC
		DVIPS:=pdvips
else
		DVIPS:=pvipsk
endif


DOC_DIR=doc

$(DOC_DIR)/$(RESULT)/figure:
	mkdir -p $@

$(DOC_DIR)/$(RESULT)/figure/dependence.dot: $(DOC_DIR)/$(RESULT)/figure $(DOC_FILES)
	$(QUIET)pp=`sed -n -e '/^#/d' -e 's/(\*pp \([^*]*\) \*)/\1/p;q' $(FIRST_DOC_FILE)`; \
	if [ -z "$$pp" ]; then \
	  $(ECHO) $(REAL_OCAMLFIND) $(OCAMLDOC) $(OCAML_FIND_PACKAGES) -dot -dot-include-all -dot-reduce -o $@ $(OCAMLDOCFLAGS) $(INCFLAGS) $(DOC_FILES); \
	  $(REAL_OCAMLFIND) $(OCAMLDOC) $(OCAML_FIND_PACKAGES) -dot -dot-include-all -dot-reduce -o $@ $(OCAMLDOCFLAGS) $(INCFLAGS) $(DOC_FILES); \
	else \
	  $(ECHO) $(REAL_OCAMLFIND) $(OCAMLDOC) $(OCAML_FIND_PACKAGES) -pp \"$$pp $(PPFLAGS)\" -dot -dot-include-all -dot-reduce -o $@ $(OCAMLDOCFLAGS) \
	  	$(INCFLAGS) $(DOC_FILES); \
	  $(REAL_OCAMLFIND) $(OCAMLDOC) $(OCAML_FIND_PACKAGES) -pp "$$pp $(PPFLAGS)" -dot -dot-include-all -dot-reduce -o $@ $(OCAMLDOCFLAGS) \
	  	$(INCFLAGS) $(DOC_FILES); \
	fi

dotdoc:	$(DOC_DIR)/$(RESULT)/figure/dependence.dot

.PHONY: miscdoc
miscdoc: ladoc
	cd doc ; \
	$(MAKE)

.PHONY: clean-miscdoc
clean-miscdoc:
	cd doc ; \
	$(MAKE) clean

OCAMLDOCFLAGS=-noheader -notrailer
include OCamlMakefile

doc: htdoc ladoc dotdoc miscdoc
clean-doc:: clean-miscdoc

