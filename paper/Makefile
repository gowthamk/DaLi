all:
	pdflatex -shell-escape dali
	pdflatex -shell-escape dali
	bibtex dali
	bibtex dali
	pdflatex -shell-escape dali
	bibtex dali
	pdflatex -shell-escape dali

# Check style:
proof:
	echo "weasel words: "
	sh bin/weasel *.tex
	echo
	echo "passive voice: "
	sh bin/passive *.tex
	echo
	echo "duplicates: "
	perl bin/dups *.tex


haste:
	pdflatex -shell-escape dali

clean:
	rm -f *.log *.aux *.bbl *.out *~
