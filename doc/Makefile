
LATEX=platex -halt-on-error -shell-escape
DVIPDF=dvipdfmx
DVIPDFOPT=-f ~/documents/map/ptex-ipa-epson.map -f ~/documents/map/utf-ipa-epson.map -f ~/documents/map/otf-ipa-epson.map
PSPDF=ps2pdf12
DOT=dot
SUFFIXES=.tex .dvi .bib .bbl .aux .toc .lol .lof .lot .log .blg .idx
SUFFIXES+=.pdf .ps .ps2 .dot

ifdef COMSPEC
	# for pTeX-Live 2009
	DVIPS=pdvips
	BIBTEX=pbibtex
	MAKEINDEX=mendex
	ASY = asy -dir /usr/local/share/asymptote -config "" -render=0 -texcommand=$(LATEX) -dvips=$(DVIPS) -gs="/usr/bin/gs" -psviewer="echo"
else
	# for unix
	DVIPS=pvipsk
	BIBTEX=jbibtex
	MAKEINDEX=mendex
endif

TARGETS=spec-language.ps spec-compiler.pdf
DEPENDS=$(patsubst %,%.dep, $(TARGETS))
TARGETBIB=$(patsubst %,%.bbl,$(basename $(TARGETS)))
BIBSOURCE=reference.bib

.PHONY: all beep depends

all: Makedep.mk all-depends $(TARGETS)

depends: $(DEPENDS)

beep:
	echo -e ''

###
# Graphics
#

%.eps:	%.asy
	$(ASY) -f eps $< -o $@
%.eps.deplist: %.asy
	echo $< > $@ 

%.pdf:	%.asy
	$(ASY) -f pdf -noprc $< -o $@
%.pdf.deplist: %.asy
	echo $< > $@

%.eps: %.dot
	$(DOT) $(DOTOPT) -Teps $< -o $@
%.eps.deplist: %.dot
	echo $< > $@

%.ps2: %.dot
	$(DOT) $(DOTOPT) -Tps2 $< -o $@
%.ps2.deplist: %.dot
	echo $< > $@

%.pdf: %.ps2
	$(PSPDF) $(PSPDFOPT) $< $@
%.pdf.deplist: %.ps2
	echo $< > $@

#%.dot.deplist:: %.dot
#	touch $@
#
#%.eps.deplist:: %.eps
#	touch $@
#
#%.ps.deplist:: %.ps
#	touch $@
#
#%.ps2.deplist:: %.ps2
#	touch $@

###
# TeX
#

%.pdf: %.dvi
	$(DVIPDF) $(DVIPDFOPT) $*
%.pdf.deplist: %.dvi
	echo $< > $@


%.ps: %.dvi
	$(DVIPS) $(DVIPSOPT) $*
%.ps.deplist: %.dvi
	echo $< > $@

.PRECIOUS: %.aux
%.aux:: %.tex
	touch $@

.PRECIOUS: %.bbl
%.bbl: %.aux %.tex.dep
	grep -e '^\\\(citation\|bibstyle\|bibdata\|bibcite\){' $*.aux > $*.auxdigest.new ; \
	sed -e '/^\\bibdata{/!d' -e 's/^\\bibdata{\([^}]*\)}/\1/' -e 's/.bib$$//' -e 's/$$/.bib/' $*.aux | xargs ls -goL1 --time-style=full-iso >> $*.auxdigest.new ; \
	if [ -f $@ -a -f $*.auxdigest ] ; \
		then if diff -q $*.auxdigest.new $*.auxdigest ; \
			then echo 'Need not to process bibliography data.' ; \
			else $(BIBTEX) $* ; \
			fi \
		else $(BIBTEX) $* ; \
	fi ; \
	mv $*.auxdigest.new $*.auxdigest
%.bbl.deplist: %.tex
	echo $< > $@


%.dvi: %.tex

%.dvi %.aux: %.tex %.bbl %.tex.dep
	$(LATEX) $* ; \
	while grep -e 'LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.' $(*F).log ; \
			do \
			$(LATEX) $* ; \
			done ; \
	if [ ! '$(*F)' = '$*' ] ; then \
	mv $(*F).dvi $*.dvi ; fi

%.dvi.deplist: %.tex
	echo $< > $@

%.tex.deplist:: %.tex
	sed -e 's/%.*$$//' $< | sed -e 's/\\input{[^}]*}/\n&\n/' -e '/\\input{/!d' -e 's/\\input{\([^}]*\)}/\1/g' | sed -e 's/\.tex$$//' -e "/^[[:blank:]]*$$/d" -e 's/.*/&.tex/' > $@
	sed -e 's/%.*$$//' $< | sed -e 's/\(\\includegraphics\)\(\[[^]]*\]\)\?\({[^}]*}\)/\n\1\3\n/' -e '/\\includegraphics{/!d' -e 's/\\includegraphics{\([^}]*\)}/\1/g' | sed -e '/^[[:blank:]]*$$/d' >> $@
	sed -e 's/%.*$$//' $< | sed -e 's/\\bibliography{[^}]*}/\n&\n/' -e '/\\bibliography{/!d' -e 's/\\bibliography{\([^}]*\)}/\1/g' | sed -e 's/\.bib$$//' -e "/^[[:blank:]]*$$/d" -e 's/.*/&.bib/' >> $@

%.bib.deplist:: %.bib
	touch $@

###
# Dependency solution
#
# Attention: No files that lack extentions can be acceptable.

include Makedep.mk
.PRECIOUS: %.dep
Makedep.mk: Makedep.mk.template Makefile
	echo > $@ ; \
	for suffix in $(SUFFIXES) ; \
	do  \
		sed -e "s/@@ext@@/$${suffix}/g" $< >> $@ ; \
	done

#	sed -e 's/^.*$$/&.deplist/' $< | xargs $(MAKE)

###
# Cleaning
#

.PHONY: clean-dep
clean-dep:
	rm -f $(DEPFILES)

.PHONY: clean
clean: clean-dep
	for nm in $(basename $(TARGETS)); do \
		for suf in .aux .auxdigest .dvi .blg .bbl .aux .toc .tof .tol .log .dep .deplist; do \
			rm -f $${nm}$${suf} ; \
		done ; \
	done
	rm -f $(TARGETS)

DEPFILES:=$(DEPENDS)
-include $(DEPFILES)
all-depends: $(DEPFILES)

