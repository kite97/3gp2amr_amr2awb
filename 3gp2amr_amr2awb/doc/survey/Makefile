target = audioplay.pdf audiorecord.pdf  digitcollection.pdf meeting.pdf \
	 technicalcompare.pdf image.pdf amr.pdf

all: $(target)

clean:
	rm -f *.log *.aux
	rm -f $(target)
%.pdf: %.tex
	xelatex $<
