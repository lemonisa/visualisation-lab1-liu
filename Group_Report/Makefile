# Make File for creating Tex documents in Linux with bibliographies in the .bib form.
# Made by Nat Welch
#
# Targets:
#
#   ps       --- builds the postscript version of the document
#   pdf      --- builds the pdf version of the document
#   all      --- builds both the pdf and ps versions of the document
#   clean --- cleans up everything that can be regenerated

# Your Shell
SH        = /bin/bash

# Your Tex files
TEXFILES  = Lab_1_Group_8.tex

# Your Bib files
#BIBFILES  = labreport.bib

# The name of your compiled files (x.pdf and x.ps)
NAME      = Lab_1_Group_8

# The location of your tex compiler
LATEX     = latex

#BIBTEX    = bibtex
PS        = dvips

# Compile flags
PSFLAGS   = -f -t letter -Ppdf

# Begin target definitions
all: ps pdf smallclean

ps: $(NAME).ps

pdf: $(NAME).pdf

%.ps: %.dvi
	$(PS) $(PSFLAGS) $(NAME).dvi > $(NAME).ps

%.pdf:
	pdflatex $(TEXFILES) && pdflatex $(TEXFILES)


%.dvi:
	#@echo " --- Running LaTeX"
	#$(LATEX) $(TEXFILES)
	#@echo " --- Running BibTeX"
	#$(BIBTEX) $(NAME)
	@echo "	Running LaTeX until complete"
	if $(LATEX) $(TEXFILES) | tee /dev/tty | fgrep -is rerun; then \
		@echo "	--- Rerunning LaTeX";				\
		while $(LATEX) $(TEXFILES) | fgrep -is rerun; do	\
			@echo "  Re-Rerunning LaTeX";			\
		done;							\
	fi

%.tex:
	@/bin/true

smallclean:
	rm -f $(NAME).dvi $(NAME).log $(NAME).aux *~ *.bbl *.blg *.toc
	rm -f $(NAME).bbl $(NAME).blg $(NAME).synctex.gz

clean: smallclean
	rm -f $(NAME).ps $(NAME).pdf

