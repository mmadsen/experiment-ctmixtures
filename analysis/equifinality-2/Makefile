.PHONY : clean

pdf:
	../knit pdf equifinality-2-randomforests.Rmd
	/Applications/RStudio.app/Contents/MacOS/pandoc/pandoc equifinality-2-randomforests.utf8.md --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash-implicit_figures --output equifinality-2-randomforests.tex --filter /Applications/RStudio.app/Contents/MacOS/pandoc/pandoc-citeproc --table-of-contents --toc-depth 2 --template /Library/Frameworks/R.framework/Versions/3.1/Resources/library/rmarkdown/rmd/latex/default.tex --number-sections --highlight-style tango --latex-engine xelatex --variable geometry:margin=1in --bibliography ../../paper/madsen2015-ctmixtures.bib 
	/Applications/RStudio.app/Contents/MacOS/pandoc/pandoc equifinality-2-randomforests.utf8.md --to latex --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash-implicit_figures --output equifinality-2-randomforests.pdf --filter /Applications/RStudio.app/Contents/MacOS/pandoc/pandoc-citeproc --table-of-contents --toc-depth 2 --template /Library/Frameworks/R.framework/Versions/3.1/Resources/library/rmarkdown/rmd/latex/default.tex --number-sections --highlight-style tango --latex-engine xelatex --variable geometry:margin=1in --bibliography ../../paper/madsen2015-ctmixtures.bib 

clean:
	rm -rf *.log *.pdf *.html *.md *.tex *_cache *_files
