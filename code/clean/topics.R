rm(list=ls())

library(dplyr)
library(ggplot2)
library(quanteda)
require(quanteda.textmodels)
library(tidyverse)
library(gofastr)

parl_tokens <- read_rds("data/intermed/parliament_tokens.rds")

# parl_tokens %>% quanteda::as.list() %>% View()

get_topic_bills <- function(kwic_obj, tokens = parl_tokens){
  data_subset <- tokens_subset(tokens, docnames(tokens) %in% kwic_obj$docname) %>% 
    docvars() %>% 
    select(bill_title) %>% 
    drop_na() %>% 
    unique()
  
  return(data_subset$bill_title)
}

apply_occ_th <- function(tokens, kw_tokens){
  kw_counter <- tokens %>% 
    tokens_select(kw_tokens, valuetype = 'glob', selection = 'keep') %>% 
    dfm() %>% 
    dfm_group(bill_title) %>% 
    rowSums()
  
  kw_counter <- kw_counter %>% 
    subset(kw_counter > 5)
  
  return(kw_counter %>% names())
}

# LMBTQ -------------------------------------------------------------------

kwic(parl_tokens, pattern = 'férfi', window = 3)

lmbtq.tokens <- phrase(c('meleg pár*', 'melegek*', 'meleg férfi*', 'meleg nő*', 'lmbtq*', 'homosz*', 'homof*', 'interszex*'))
lmbtq.kwic <- kwic(parl_tokens, pattern = lmbtq.tokens, window = 3, valuetype = 'glob')
lmbtq.kwic


parl_tokens.lgbtq <- tokens_subset(parl_tokens, docvars(parl_tokens)$bill_title %in% get_topic_bills(lmbtq.kwic))

pelda.dfm <- pelda %>% tokens_select(lmbtq.tokens, valuetype = 'glob', selection = 'keep') %>% dfm()
pelda.dfm %>% dfm_group(bill_title) %>% rowSums() %>% as.data.frame() %>% View()

# Migration ---------------------------------------------------------------

migrant.tokens <- phrase(c('migr*', 'menek*', 'beván*'))
migrant.kwic <- kwic(parl_tokens, pattern = migrant.tokens, window = 2, valuetype = 'glob')

parl_tokens.migration <- tokens_subset(parl_tokens, docvars(parl_tokens)$bill_title %in% get_topic_bills(migrant.kwic))

parl_tokens.migration <- tokens_subset(parl_tokens.migration, docvars(parl_tokens.migration)$bill_title %in% apply_occ_th(parl_tokens.migration, migrant.tokens))

parl_tokens.migration %>% 
  write_rds("data/intermed/parliament_tokens_migration.rds")

# change the unit to paragraph
# keep the paragraphs with keywords

# Religion ----------------------------------------------------------------

religion.tokens <- phrase(c('^vallás*', '*hitük', "hitvallás", 'keresztény', "muszlim", "muzulmán", "zsidó", "katolikus", "^egyház", "református"))
religion.kwic <- kwic(parl_tokens, pattern = religion.tokens, window = 2, valuetype = 'glob')

parl_tokens.religion <- tokens_subset(parl_tokens, docvars(parl_tokens)$bill_title %in% get_topic_bills(religion.kwic))

parl_tokens.religion <- tokens_subset(parl_tokens.religion, docvars(parl_tokens.religion)$bill_title %in% apply_occ_th(parl_tokens.religion, religion.tokens))

parl_tokens.religion %>% 
  write_rds("data/intermed/parliament_tokens_religion.rds")

# Global warming ----------------------------------------------------------

environment.tokens <- phrase(c('felmeleg*', 'környezetvéd*', 'károsanyag*', 'klímavált*'))
environment.kwic <- kwic(parl_tokens, pattern = environment.tokens, window = 2, valuetype = 'glob')

parl_tokens.environ <- tokens_subset(parl_tokens, docvars(parl_tokens)$bill_title %in% get_topic_bills(environment.kwic))

parl_tokens.environ <- tokens_subset(parl_tokens.environ, docvars(parl_tokens.environ)$bill_title %in% apply_occ_th(parl_tokens.environ, environment.tokens))

parl_tokens.environ %>% 
  write_rds("data/intermed/parliament_tokens_environment.rds")