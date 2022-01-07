R = R CMD BATCH
STOPWORDS = data/stopwords/stopwords-hu.txt data/stopwords/stopwords-parliament.txt data/stopwords/stopphrases-parliament.txt
YEARS = $(shell seq 2010 2021)

figures/slant_estimates_origo_case.png figures/slant_estimate_magyar_nemzet_case.png&: code/plot_case_studies.py $(foreach year, $(YEARS), data/output/Q_slant_pred_$(year).csv)
	python3 -b $<

$(foreach year, $(YEARS), data/output/Q_slant_pred_$(year).csv)&: code/predict_media_slant.R data/output/wordscore_fit.rds data/output/selected_phrases.rds $(foreach year, $(YEARS), data/output/media_corpus_$(year).rds) 
	$(R) $< logs/predict_media_slant.Rout

data/output/wordscore_fit.rds: code/train_wordscore_model.R data/output/parliament_tokens.rds data/output/selected_phrases.rds
	$(R) $< logs/train_wordscore_model.Rout

data/output/selected_phrases.rds: code/create_selected_phrases.R data/output/parliament_tokens.rds
	$(R) $< logs/create_selected_phrases.Rout

data/output/parliament_tokens.rds: code/create_parliament_tokens.R data/input/parlament_speech_2014-2018.csv $(STOPWORDS)
	$(R) $< logs/create_parliament_tokens.Rout

$(foreach year, $(YEARS), data/output/media_corpus_$(year).rds)&: code/create_year_media_corpuses.R data/raw/media_corpus_raw.csv
	$(R) $< logs/create_year_media_corpuses.Rout