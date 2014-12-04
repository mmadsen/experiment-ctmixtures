.PHONY : clean

all:
	Rscript pop-sampled-classification-analysis.r $(datadir)
	Rscript tasampled-classification-analysis.r $(datadir)
	Rscript combined-tasampled-duration-hidden-analysis.r $(datadir)
	Rscript bias-model-classification-analysis.r $(datadir)


clean:
	rm -rf *.log *.pdf *.html *.md *.tex *_cache *_files
