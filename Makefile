# Build the R Journal-style paper.
#
# Layout expected:
#   paper/
#   ├── make_figures.R       # regenerates figures
#   ├── figures/             # PDFs produced by make_figures.R
#   ├── tables/              # .tex tables produced by make_figures.R
#   └── rj/
#       ├── paper.Rmd        # source
#       ├── header.tex       # preamble
#       ├── RJournal.sty     # patched R Journal class
#       ├── RJwrapper.tex    # outer wrapper
#       ├── RJreferences.bib # bibliography
#       └── figures/         # mirrored from ../figures
#
# Usage:
#   make         # regenerate figures then render PDF
#   make pdf     # render PDF only
#   make clean   # remove build artefacts

RSCRIPT ?= Rscript
PANDOC_DIR ?= /Applications/quarto/bin/tools

.PHONY: all figures pdf clean

all: figures pdf

figures:
	RSTUDIO_PANDOC=$(PANDOC_DIR) $(RSCRIPT) paper/make_figures.R
	cp -f paper/figures/*.pdf paper/rj/figures/ 2>/dev/null || true
	cp -f paper/tables/*.tex paper/rj/tables/ 2>/dev/null || true

pdf:
	cd paper/rj && RSTUDIO_PANDOC=$(PANDOC_DIR) $(RSCRIPT) -e \
	  'rmarkdown::render("paper.Rmd", output_format = rticles::rjournal_article(), quiet = TRUE)'

clean:
	rm -f paper/rj/RJwrapper.pdf paper/rj/paper.tex paper/rj/paper.R
