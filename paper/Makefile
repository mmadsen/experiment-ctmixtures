.PHONY : clean

pdf:
	./knit pdf
	pandoc -s --biblio madsen2015-ctmixtures.bib madsen2015-ctmixtures.md --template=elsarticle-template.tex --no-tex-ligatures --latex-engine=xelatex -o madsen2015-ctmixtures.tex --natbib --number-sections --listings
	#pandoc -s --smart --biblio madsen2015-ctmixtures.bib madsen2015-ctmixtures.md --template=elsarticle-template.tex -o madsen2015-ctmixtures.tex --natbib --number-sections --listings
	latexmk -pdf madsen2015-ctmixtures
	open -a /Applications/Skim.app madsen2015-ctmixtures.pdf


github:
	./knit github
	pandoc -s -S --biblio madsen2015-ctmixtures.bib madsen2015-ctmixtures.md -o madsen2015-ctmixtures_github.md

md:
	pandoc -s -S --biblio madsen2015-ctmixtures.bib madsen2015-ctmixtures.md -o madsen2015-ctmixtures.md

odt:
	./knit docx
	pandoc -s -S --biblio madsen2015-ctmixtures.bib madsen2015-ctmixtures.md --reference-odt=reference.odt -o madsen2015-ctmixtures.odt


docx:
	./knit docx
	pandoc -s -S --biblio madsen2015-ctmixtures.bib madsen2015-ctmixtures.md --reference-docx=reference.docx -o madsen2015-ctmixtures.docx



epub:
	./knit epub
	pandoc -s -S --biblio madsen2015-ctmixtures.bib madsen2015-ctmixtures.md -o madsen2015-ctmixtures.epub


clean:
	latexmk -CA
	rm -rf *.log *.bbl *.blg *.out *.md *.pdf *.docx madsen2015-ctmixtures.tex *.epub
