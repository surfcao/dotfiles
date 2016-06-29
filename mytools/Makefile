MAINTEX=main
MAINTEX_NEW=main2
DIFFTEX=diff

LATEX=pdflatex
BIBTEX=bibtex
PDFVIEWER=zathura
LATEXDIFF=latexdiff

OUTDIR=./output/

#$(@D) indicates the path where the target resides in
#CREATE_OUTDIR=@mkdir -p $(@D)

LATEXFLAGS= -output-directory=${OUTDIR}
#DIFF_FLAGS="--flatten, -t CTRADITIONAL"

#echo -e"\033[32m begin compile"${MAINTEX}
#echo -e -n"\033[0m "


all: ${OUTDIR}${MAINTEX}.pdf
	${LATEX} ${LATEXFLAGS} $(MAINTEX)

${OUTDIR}${MAINTEX}.pdf:${MAINTEX}.tex
	@mkdir -p $(@D)
	${LATEX} ${LATEXFLAGS} ${MAINTEX}
	${BIBTEX} ${OUTDIR}$(MAINTEX)
	${LATEX} ${LATEXFLAGS} $(MAINTEX)
	${LATEX} ${LATEXFLAGS} $(MAINTEX)

diff: ${OUTDIR}${DIFFTEX}.pdf

${OUTDIR}${DIFFTEX}.pdf: ${DIFFTEX}.tex
	@mkdir -p ${OUTDIR}
	${LATEX} ${LATEXFLAGS} ${DIFFTEX}
	${BIBTEX} ${OUTDIR}$(DIFFTEX)
	${LATEX} ${LATEXFLAGS} $(DIFFTEX)
	${LATEX} ${LATEXFLAGS} $(DIFFTEX)

${DIFFTEX}.tex:
	${LATEXDIFF} ${DIFF_FLAGS} ${MAINTEX}.tex ${MAINTEX_NEW}.tex > ${DIFFTEX}.tex

view: ${OUTDIR}${MAINTEX}.pdf
	${PDFVIEWER} ${OUTDIR}${MAINTEX}.pdf &

viewdiff: ${OUTDIR}${DIFFTEX}.pdf
	${PDFVIEWER} ${OUTDIR}${DIFFTEX}.pdf &

clean: 
	rm -rf ${OUTDIR}

#echo -e"\033[32m successfully!"
#echo -e -n"\033[0m "

