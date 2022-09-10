renv::activate()
rm(list = ls())

library(dplyr)
library(ggplot2)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidyverse)
library(gofastr)
library(writexl)

create_ngramm <- function(toks, ngram, min_termfreq){
  ngram_keyness <- tokens_ngrams(parl_tokens, n = ngram) %>% 
    dfm() %>%
    dfm_group(groups = side) %>%
    dfm_trim(groups = side, min_termfreq = min_termfreq) %>% 
    textstat_keyness(target = 1, measure = "chi2")
  return(ngram_keyness)
}

create_phrases <- function(bigrams, trigrams, first_n = 500, last_n = 500){
  bigrams <- bigram_keyness %>% mutate(feature = str_replace_all(feature, "_", " "))
  trigrams <- trigram_keyness %>% mutate(feature = str_replace_all(feature, "_", " "))
  
  bigrams <- rbind(head(bigrams, first_n), tail(bigrams, last_n))
  trigrams <- rbind(head(trigrams, first_n), tail(trigrams, last_n))
  
  p <- data.frame(cbind(c(bigrams$feature, trigrams$feature)))
  colnames(p)[1] <- "p"
  p$p <- str_replace_all(p$p, " ", "_")
  selected_ps <- prep_stopwords(p %>% select(p))
  return(selected_ps)
}

save_phrases <- function(bigrams, trigrams, path, first_n = 60, last_n = 60){
  bigrams <- bigrams %>% mutate(feature = str_replace_all(feature, "_", " "))
  trigrams <- trigrams %>% mutate(feature = str_replace_all(feature, "_", " "))
  
  bigrams <- rbind(head(bigrams, first_n), tail(bigrams, last_n))
  trigrams <- rbind(head(trigrams, first_n), tail(trigrams, last_n))
  
  features_table <- cbind(bigrams, trigrams)
  features_table <- cbind(head(features_table, 60), tail(features_table, 60))
  features_table %>% write_xlsx(path)
}

# Migration ---------------------------------------------------------------

parl_tokens <- read_rds("data/intermed/parliament_tokens_migration.rds")

bigram_keyness <- create_ngramm(parl_tokens, 2, 20)
trigram_keyness <- create_ngramm(parl_tokens, 3, 5)

selected_ps <- create_phrases(bigram_keyness, trigram_keyness)
selected_ps %>% write_rds("data/intermed/selected_phrases_migration.rds")

save_phrases(bigram_keyness, trigram_keyness, "data/intermed/top_khi_phrases_raw_migration.xlsx")

# Religion ----------------------------------------------------------------

parl_tokens <- read_rds("data/intermed/parliament_tokens_religion.rds")

bigram_keyness <- create_ngramm(parl_tokens, 2, 20)
trigram_keyness <- create_ngramm(parl_tokens, 3, 5)

selected_ps <- create_phrases(bigram_keyness, trigram_keyness)
selected_ps %>% write_rds("data/intermed/selected_phrases_religion.rds")

save_phrases(bigram_keyness, trigram_keyness, "data/intermed/top_khi_phrases_raw_religion.xlsx")

# Environment -------------------------------------------------------------

parl_tokens <- read_rds("data/intermed/parliament_tokens_environment.rds")

bigram_keyness <- create_ngramm(parl_tokens, 2, 20)
trigram_keyness <- create_ngramm(parl_tokens, 3, 5)

selected_ps <- create_phrases(bigram_keyness, trigram_keyness)
selected_ps %>% write_rds("data/intermed/selected_phrases_environment.rds")

save_phrases(bigram_keyness, trigram_keyness, "data/intermed/top_khi_phrases_raw_environment.xlsx")

# TODO --------------------------------------------------------------------

# LDA
# Értelmes szavak
# TH a keywordre

# Validáció:
# Manifesto
# V-DEM

# ESS



quanteda::as.list(parl_tokens) %>% View()



