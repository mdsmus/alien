NAME = cl-dist-manual
TEXSRCS = $(wildcard cha*.tex)
LISPSRC =  $(wildcard lisp/*.lisp)
OTHER +=  $(patsubst %.lisp,%.tex,$(notdir $(wildcard lisp/*.lisp)))
USE_PDFLATEX = 1
clean_ext = .4ct .4tc .aux .css .dvi .html .idv .lg .log .out .tmp .xref .bbl .blg .idx
CLEAN_FILES +=  $(foreach ext, $(clean_ext), $(addsuffix $(ext), $(NAME)))
CLEAN_FILES +=  $(wildcard examples-color/*.tex)

vpath %.sty config
vpath %.lisp lisp
vpath %.tex examples-color

pdf: $(OTHER) lisp.sty

ps: $(OTHER) lisp.sty

view-pdf: pdf
	gv -page=7 -resize $(NAME).pdf

foo: $(NAME).tex
	htlatex $< "site,uni-html4" " -cunihtf"

%.tex: %.lisp
	pygmentize -f latex "$<" > examples-color/$@

include ~/.latexmk

