renv::activate()
rm(list = ls())

library(dplyr)
library(quanteda)
library(quanteda.textmodels)
quanteda_options(threads = 8)
library(tidyverse)


# Create phrase frequencies of selected phrases in parliament text
get_topic_dfm <- function(toks, phrases){
  phrase_frequency_table_parliament <- parl_tokens %>%
  tokens_ngrams(n = 2:3) %>%
  tokens_select(pattern = phrase(selected_ps), selection = "keep") %>%
  dfm() %>% 
  # dfm_trim(min_termfreq = 50, max_docfreq = .5, docfreq_type = 'prop') %>% 
  dfm_subset(ntoken(.) > 0)
  return(phrase_frequency_table_parliament)
}

# train wordscore model
train_model <- function(dfm){
  tmod_ws <- textmodel_wordfish(dfm)
  # tmod_ws <- textmodel_wordscores(dfm,
  #   y = toks$label,
  #   smooth = 0
  # )
  return(tmod_ws)
}

# Migration ---------------------------------------------------------------

parl_tokens <- read_rds("data/intermed/parliament_tokens_migration.rds")
selected_ps <- read_rds("data/intermed/selected_phrases_migration.rds")

dtm <- get_topic_dfm(parl_tokens, selected_ps)

model_wf <- dtm %>% 
  train_model()

model_wf %>% 
  write_rds("data/intermed/wordscore_fit_migration.rds")

# Religion ----------------------------------------------------------------

parl_tokens <- read_rds("data/intermed/parliament_tokens_religion.rds")
selected_ps <- read_rds("data/intermed/selected_phrases_religion.rds")

get_topic_dfm(parl_tokens, selected_ps) %>% 
  train_model() %>%  
  write_rds("data/intermed/wordscore_fit_religion.rds")

# Environment -------------------------------------------------------------

parl_tokens <- read_rds("data/intermed/parliament_tokens_environment.rds")
selected_ps <- read_rds("data/intermed/selected_phrases_environment.rds")

get_topic_dfm(parl_tokens, selected_ps) %>% 
  train_model() %>%  
  write_rds("data/intermed/wordscore_fit_environment.rds")


