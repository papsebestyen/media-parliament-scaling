renv::activate()
rm(list=ls())

library(dplyr)
library(quanteda)
library(quanteda.textmodels)
quanteda_options(threads = 8)
library(tidyverse)

# Create phrase frequencies of selected phrases in parliament text
get_topic_dfm <- function(toks, phrases){
  phrase_frequency_table_parliament <- toks %>%
  tokens_ngrams(n=2:3) %>%
  tokens_select(pattern = phrase(phrases), selection = "keep") %>% 
  dfm() %>% 
  dfm_subset(ntoken(.) > 0)
  return(phrase_frequency_table_parliament)
}

correct_meta <- function(fit, meta_toks){
  predicted_slant <- as.data.frame(fit)
  predicted_slant <- cbind(party_quarter = rownames(predicted_slant), predicted_slant)
  rownames(predicted_slant) <- 1:nrow(predicted_slant)
  
  predicted_slant <- cbind(
    predicted_slant,
    speaker = docvars(meta_toks)$speaker,
    year = docvars(meta_toks)$year,
    side = docvars(meta_toks)$side,
    party = docvars(meta_toks)$speaker_party
  )
  return(predicted_slant)
}

# Migration ---------------------------------------------------------------

parl_tokens <- read_rds("data/intermed/parliament_tokens_migration.rds")
selected_ps <- read_rds("data/intermed/selected_phrases_migration.rds")
tmod_ws <- read_rds("data/intermed/wordscore_fit_migration.rds")

  # dfm_trim(min_termfreq = 50, max_docfreq = .5, docfreq_type = 'prop') %>%
  # dfm_subset(ntoken(.) > 0)
phrase_frequency_table_parliament <- get_topic_dfm(parl_tokens, selected_ps)
pred_ws <- predict(tmod_ws, se.fit = TRUE, newdata = phrase_frequency_table_parliament)
pred_ws <- summary(tmod_ws)$estimated.document.positions
predicted_slant <- correct_meta(pred_ws, phrase_frequency_table_parliament)
  
predicted_slant %>% write_csv("data/slant_estimates/speech_slant_pred_migration.csv")

# Religion ----------------------------------------------------------------

parl_tokens <- read_rds("data/intermed/parliament_tokens_religion.rds")
selected_ps <- read_rds("data/intermed/selected_phrases_religion.rds")
tmod_ws <- read_rds("data/intermed/wordscore_fit_religion.rds")

phrase_frequency_table_parliament <- get_topic_dfm(parl_tokens, selected_ps)
pred_ws <- predict(tmod_ws, se.fit = TRUE, newdata = phrase_frequency_table_parliament)
pred_ws <- summary(tmod_ws)$estimated.document.positions
predicted_slant <- correct_meta(pred_ws, phrase_frequency_table_parliament)
  
predicted_slant %>% write_csv("data/slant_estimates/speech_slant_pred_religion.csv")

# Environment -------------------------------------------------------------

parl_tokens <- read_rds("data/intermed/parliament_tokens_environment.rds")
selected_ps <- read_rds("data/intermed/selected_phrases_environment.rds")
tmod_ws <- read_rds("data/intermed/wordscore_fit_environment.rds")

phrase_frequency_table_parliament <- get_topic_dfm(parl_tokens, selected_ps)
pred_ws <- predict(tmod_ws, se.fit = TRUE, newdata = phrase_frequency_table_parliament)
pred_ws <- summary(tmod_ws)$estimated.document.positions
predicted_slant <- correct_meta(pred_ws, phrase_frequency_table_parliament)
  
predicted_slant %>% write_csv("data/slant_estimates/speech_slant_pred_environment.csv")
