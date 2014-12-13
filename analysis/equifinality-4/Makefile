.PHONY : clean

all:
	Rscript population-classification.r $(datadir)
	Rscript tasampled-classification-analysis.r $(datadir)
	Rscript combined-tasampled-duration-hidden-analysis.r $(datadir)
	Rscript perlocus-tasampled-classification-analysis.r $(datadir)
	
	Rscript bias-model-classification-analysis.r $(datadir)
	Rscript bias-neutral-comparison.r $(datadir)
	Rscript perlocus-analysis.r $(datadir)


clean:
	rm -rf *.log *.pdf *.html *.md *.tex *_cache *_files
