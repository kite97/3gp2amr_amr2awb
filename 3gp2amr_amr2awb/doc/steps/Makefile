target = summary_trying.pdf summary.pdf

all: $(target)

clean:
	rm -f *.log *.aux
	rm -f $(target)
%.pdf: %.tex
	xelatex $<
