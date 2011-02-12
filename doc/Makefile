
LATEX=platex -halt-on-error -shell-escape
DVIPDF=dvipdfmx
DVIPDFOPT=-f ~/documents/map/ptex-ipa-epson.map -f ~/documents/map/utf-ipa-epson.map -f ~/documents/map/otf-ipa-epson.map
ifdef COMSPEC
	# for pTeX-Live 2009
	DVIPS=pdvips
	BIBTEX=pbibtex
	MAKEINDEX=mendex
else
	# for unix
	DVIPS=pvipsk
	BIBTEX=jbibtex
	MAKEINDEX=mendex
endif

TARGETS=spec-language.ps spec-compiler.pdf
DEPENDS=$(patsubst %,%.dep,$(basename $(TARGETS)))
TARGETBIB=$(patsubst %,%.bbl,$(basename $(TARGETS)))
BIBSOURCE=reference.bib

.PHONY: all beep depends

all: $(TARGETBIB) $(TARGETS)

depends: $(DEPENDS)

beep:
	echo -e ''

%.pdf: %.dvi
	$(DVIPDF) $(DVIPDFOPT) $*

%.ps: %.dvi
	$(DVIPS) $(DVIPSOPT) $*

.PRECIOUS: %.aux
%.aux:: %.tex
	$(LATEX) $*

%.bbl: %.aux $(BIBSOURCE)
	grep -e '^\\\(citation\|bibstyle\|bibdata\|bibcite\)' $*.aux > $*.auxdigest.new ; \
	if [ -f $@ -a -f $*.auxdigest ] ; \
		then if diff -q $*.auxdigest.new $*.auxdigest ; \
			then echo 'Need not to process bibliography data.' ; \
			else $(BIBTEX) $* ; \
			fi \
		else $(BIBTEX) $* ; \
	fi ; \
	mv $*.auxdigest.new $*.auxdigest

%.deplist: %.tex
	cat $*.tex | sed -e 's/%/\n%/' | sed -e '/^%/d' | sed -e 's/\\input{[^}]*}/\n&\n/' -e '/\\input/!d' -e 's/\\input{\([^}]*\)}/\1/g' | sed -e 's/\.tex$$//' -e "/^[[:blank:]]*$$/d" -e 's/.*/&.tex/' > $@
#	cat $*.tex | sed -e 's/%/\n%/' | sed -e '/^%/d' | sed -e 's/\(\\includegraphics\)\(\[[^]]*\]\)\?\({[^}]*}\)/\n\1\3\n/' -e '/\\includegraphics/!d' -e 's/\\includegraphics{\([^}]*\)}/\1/g' | sed -e '/^[[:blank:]]*$$/d' >> $@

# Attention: No files that lack extentions can be acceptable.

.PRECIOUS: %.dep
%.dep: %.deplist
	sed -e '/^[[:blank:]]*$$/d' -e 's/^\(.*\)\.[^.]*$$/\1.dep/' $< | sed -e '1i$*.dep: $*.tex ' | paste -s '-d ' > $@
	sed -e '/^[[:blank:]]*$$/d' -e 's/^\(.*\)\.[^.]*$$/\1.dep/' $< | sed -e '1i-include ' | paste -s '-d ' >> $@

%.dep:: %.pdf
	touch $@

#	sed -e 's/^.*$$/&.deplist/' $< | xargs $(MAKE)
%.dvi: %.tex

%.dvi: %.tex %.bbl %.aux %.dep
	$(LATEX) $* ; \
	if grep -e 'LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.' $*.log ; then \
		$(LATEX) $* ; \
	fi
#	while grep -e 'LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.' $*.log ; \
#			do \
#			$(LATEX) $* ; \
#			done

.PHONY: clean-dep
clean-dep:
	rm -f *.dep *.deplist

.PHONY: clean
clean: clean-dep
	for nm in $(basename $(TARGETS)); do \
		for suf in .aux .auxdigest .dvi .blg .bbl .aux .toc .tof .tol .log .dep .deplist; do \
			rm -f $${nm}$${suf} ; \
		done ; \
	done
	rm -f $(TARGETS)

-include $(DEPENDS)

