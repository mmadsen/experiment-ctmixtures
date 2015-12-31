.PHONY : clean

pdf:
	latexmk -pdf -pv madsen2015-ctmixtures
	open -a /Applications/Skim.app madsen2015-ctmixtures.pdf

cont:
	latexmk -pdf -pvc madsen2015-ctmixtures

clean:
	latexmk -CA
	rm -rf *.log *.bbl *.blg *.out *.pdf 
