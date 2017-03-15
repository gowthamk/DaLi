all:
	pdflatex -shell-escape vml
	pdflatex -shell-escape vml
	bibtex vml
	bibtex vml
	pdflatex -shell-escape vml
	bibtex vml
	pdflatex -shell-escape vml

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
	pdflatex -shell-escape vml

clean:
	rm -f *.log *.aux *.bbl *.out *~
