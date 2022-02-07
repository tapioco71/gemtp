# -*- mode:makefile-gmake; coding:utf-8; indent-tabs-mode:t -*-

VERBOSE=
LISP=$(shell which sbcl)
LISP_FLAGS=--no-sysinit --no-userinit
LISP_LOAD_OPTION=--load
LISP_LOAD_OPTION=--eval
PDF_VIEWER=/usr/bin/xpdf
WGET=$(shell which wget)
WGET_OPTIONS=
FIG2DEV=$(shell which fig2dev)
FIG2DEV_OPTIONS=-L png -S 4
SED=$(shell which sed)
TOP=$(shell git rev-parse --show-toplevel)
MAKE_RST_DEPENDS=$(TOP)/tools/make-rst-depends
RSTPRE=$(TOP)/tools/rstpre
RSTUML=$(TOP)/tools/rstuml
HELPFMT="$(shell basename $(MAKE)) %-20s \# %s\n"

.PHONY:: view clean clean-pdf clean-pdfs checkout-pdf checkout-pdfs documents

view: $(PDFS)
	@if [ $(PDF_VIEWER) = open ] ; then 	\
		$(PDF_VIEWER) $^ ;		\
	 else 					\
		for i in $^ ; do 		\
			$(PDF_VIEWER) $$i & 	\
		done ;				\
	 fi

clean:: clean-pdf clean-png

clean-pdf clean-pdfs:
	@if [ 'x${VERBOSE}' = x ];		\
	then					\
		echo " [ clean ] pdf files";	\
	else					\
		echo " -rm -f $(PDFS) ";	\
	fi
	@rm -f $(PDFS)

clean-png clean-pngs:
	@if [ 'x${VERBOSE}' = x ];		\
	then					\
		echo " [ clean ] png files";	\
	else					\
		echo " -rm -f images/*.png ";	\
	fi
	@rm -f images/*.png


# Generate the dependencies of restructuredText documents (includes
# and images).  Note: this should be done better by a script, since
# included files may themselves include other files or images.
%.d: %.txt
	@if [ 'x${VERBOSE}' = x ];			\
	then						\
		echo " [ RST ] depend for file $< ";	\
	else						\
		echo $(MAKE_RST_DEPENDS) $< > $@;	\
	fi
	@$(MAKE_RST_DEPENDS) $< > $@

# Generate PDF from reStructuredText document.
%.pdf: %.txt
	@if [ 'x${VERBOSE}' = x ];			\
	then						\
		echo " [ GEN ] pdf document $@ ";	\
	else						\
		echo " rst2pdf -o $@ $< ";		\
	fi
	@rst2pdf -o $@ $<

# Object diagrams.
o-%.png: o-%.fig
	@if [ 'x${VERBOSE}' = x ];				\
	then							\
		echo " [ FIG ] drawing object diagram $@ ";	\
	else							\
		echo " $(FIG2DEV) $(FIG2DEV_OPTIONS) $< $@ ";	\
	fi
	@$(FIG2DEV) $(FIG2DEV_OPTIONS) $< $@

# Object diagrams = Class diagrams.
o-%.png: o-%.yuml
	@if [ 'x${VERBOSE}' = x ];				\
	then							\
		echo " [ YUML ] drawing object diagram $@ ";	\
	else							\
		echo " $(WGET) $(WGET_OPTIONS) "http://yuml.me/diagram/plain/class/$$(tr '\012' ',' < $< | sed -e 's/,*$$//')" -O $@ ";	\
	fi
	@$(WGET) $(WGET_OPTIONS) "http://yuml.me/diagram/plain/class/$$(tr '\012' ',' < $< |sed -e 's/,*$$//')" -O $@

# State diagrams = Activity diagrams.
s-%.png: s-%.yuml
	@if [ 'x${VERBOSE}' = x ];				\
	then							\
		echo " [ YUML ] drawing state diagram $@ ";	\
	else							\
		echo " $(WGET) $(WGET_OPTIONS) "http://yuml.me/diagram/plain/activity/$$(tr '\012' ',' < $< |sed -e 's/,*$$//')" -O $@ ";	\
	fi
	@$(WGET) $(WGET_OPTIONS) "http://yuml.me/diagram/plain/activity/$$(tr '\012' ',' < $< |sed -e 's/,*$$//')" -O $@

# Use-case diagrams.
u-%.png: u-%.yuml
	@if [ 'x${VERBOSE}' = x ];				\
	then							\
		echo " [ YUML ] drawing use-case diagram $@ ";	\
	else							\
		echo " $(WGET) $(WGET_OPTIONS) "http://yuml.me/diagram/plain/usecase/$$(tr '\012' ',' < $< |sed -e 's/,*$$//')" -O $@ ";	\
	fi
	@$(WGET) $(WGET_OPTIONS) "http://yuml.me/diagram/plain/usecase/$$(tr '\012' ',' < $< |sed -e 's/,*$$//')" -O $@


checkout-pdf checkout-pdfs: clean-pdf
	@if [ 'x${VERBOSE}' = x ];			\
	then						\
		echo " [ GIT ] checkout ${PDFS} ";	\
	else						\
		echo " git checkout $(PDFS) ";		\
	fi
	@git checkout $(PDFS)


## Example, using rstpre and rstuml:
##
# .SUFFIXES: .in1 .txt .rst .pdf .xml .odt .html
# 
# # All the preprocessing of the documents into a rst file should go here:
# %.rst:%.txt
# 	@i="$<" ; d=$$(dirname "$$i") ; f=$$(basename "$$i") ;\
# 	echo "Preparing $$i" ;\
# 	cd "$$d" \
# 	&& $(RSTPRE) "$$f" \
# 	&& mv "$${f/.txt/.rst}"  "$${f/.txt/.in1}" \
# 	&& echo "Processing UML in $${i/.txt/.in1}" \
# 	&& $(RSTUML) "$${f/.txt/.in1}"
# 
# # Following are the rules to process rst files into the target formats:
# %.pdf:%.rst
# 	@i="$<" ; d=$$(dirname "$$i") ; f=$$(basename "$$i") ;\
# 	echo "Producing $${i/.rst/.pdf}" ;\
# 	cd "$$d" \
# 	&& $(RST2PDF)  $$( [ -r $${f/.rst/.style} ] && echo -s $${f/.rst/.style} )  -o "$${f/.rst/.pdf}"  "$$f" 
# # --verbose --very-verbose
# 
# %.odt:%.rst
# 	@i="$<" ; d=$$(dirname "$$i") ; f=$$(basename "$$i") ;\
# 	echo "Producing $${i/.rst/.odt}" ;\
# 	cd "$$d" \
# 	&& $(RST2ODT) $$( [ -r $${f/.rst/.style} ] && echo -s $${f/.rst/.style} ) "$$f" "$${f/.rst/.odt}"
# 
# %.html:%.rst
# 	@i="$<" ; d=$$(dirname "$$i") ; f=$$(basename "$$i") ;\
# 	echo "Producing $${i/.rst/.html}" ;\
# 	cd "$$d" \
# 	@f="$<" \
# 	&& $(RST2HTML) $$( [ -r $${f/.rst/.style} ] && echo -s $${f/.rst/.style} ) "$$f" "$${f/.rst/.html}"

