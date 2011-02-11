
LATEX=platex -halt-on-error #-shell-escape
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
DEPENDS=$(patsubst %,%.depend,$(basename $(TARGETS)))
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

%.dependlist: %.tex
	cat $*.tex | sed -e 's/%/\n%/' | sed -e '/^%/d' | sed -e 's/\\input{[^}]*}/\n&\n/' -e '/\\input/!d' -e 's/\\input{\([^}]*\)}/\1/g' | sed -e 's/\.tex$$//' -e "/^$$/d" > $@

%.depend: %.dependlist
	( sed -e '1i$*.depend: $*.tex ' -e '/^$$/d' -e 's/^..*$$/&.depend/' $< | paste -s '-d ' ) > $@
	( sed -e '1i-include' -e '/^$$/d' -e 's/^..*$$/&.depend/' $< | paste -s '-d ' ) >> $@

#	sed -e 's/^.*$$/&.dependlist/' $< | xargs $(MAKE)
%.dvi: %.tex

%.dvi: %.tex %.bbl %.aux %.depend
	$(LATEX) $* ; \
	if grep -e 'LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.' $*.log ; then \
		$(LATEX) $* ; \
	fi
#	while grep -e 'LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.' $*.log ; \
#			do \
#			$(LATEX) $* ; \
#			done

.PHONY: clean-depend
clean-depend:
	rm -f *.depend *.dependlist

.PHONY: clean
clean: clean-depend
	for nm in $(basename $(TARGETS)); do \
		for suf in .aux .auxdigest .dvi .blg .bbl .aux .toc .tof .tol .log .depend .dependlist; do \
			rm -f $${nm}$${suf} ; \
		done ; \
	done
	rm -f $(TARGETS)

-include $(DEPENDS)

