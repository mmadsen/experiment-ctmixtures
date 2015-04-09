.PHONY : clean

pdf:
	latexmk -pdf madsen2015-ctmixtures
	open -a /Applications/Skim.app madsen2015-ctmixtures.pdf


clean:
	latexmk -CA
	rm -rf *.log *.bbl *.blg *.out *.pdf 
