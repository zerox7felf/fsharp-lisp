# Makefile tex -> pdf

DOCUMENT=report

all: clean
	pdflatex ${DOCUMENT}.tex 2>/dev/null

word_count:
	@pdftotext ${DOCUMENT}.pdf - | wc -w

clean:
