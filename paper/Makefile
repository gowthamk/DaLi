all:
	pdflatex -shell-escape dali
	pdflatex -shell-escape dali
	bibtex dali
	bibtex dali
	pdflatex -shell-escape dali
	bibtex dali
	pdflatex -shell-escape dali

haste:
	pdflatex -shell-escape dali

clean:
	rm *.log *.aux *.bbl *.out
