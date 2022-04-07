## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE------------------------------------------------------------
library(tidyverse)
library(keywordr)

## ----echo=FALSE---------------------------------------------------------------
data.frame(
  query = character(),
  volume = integer(),
  cpc = double(),
  input = character(),
  source = character()
) |> 
  str()

## ----rows.print=25------------------------------------------------------------
input_data <- tribble(
  ~query,             ~volume,
  "seo",                96000,
  "seo ye-ji",          22000,
  "seo meaning",         6700,
  "seo services",        6400,
  "seo ye ji",           6000,
  "what is a seo",       5300,
  "seo london",          5000,
  "what is seo",         4800,
  "seo agency",          4300,
  "joe seo",             4300,
  "seo company",         4200,
  "london seo agencies", 4100,
  "seo consultant",      3500,
  "seo service",         3500,
  "seo company london",  2500,
  "seo agencies london", 2500,
  "seo agency london",   2400,
  "seo services london", 2400,
  "seo consultants",     2300,
  "local seo",           2200,
  "local seo services",  2000
)
print(input_data, n = 25)

## -----------------------------------------------------------------------------
kwr <- kwresearch(input_data, accentize = FALSE)

## -----------------------------------------------------------------------------
kwr |> kwr_queries()

## -----------------------------------------------------------------------------
recipe_file <- file.path(tempdir(), "recipes.yml")

kwr_add_pattern("ye-ji", recipe_file, recipe_type = "remove")
kwr_add_pattern("joe", recipe_file, recipe_type = "remove")

kwr <- kwr |> 
  kwr_prune(recipe_file)

## -----------------------------------------------------------------------------
kwr |> kwr_queries()

## -----------------------------------------------------------------------------
kwr |> kwr_ngrams()

## -----------------------------------------------------------------------------
kwr |> kwr_subqueries()

## -----------------------------------------------------------------------------
kwr |> kwr_collocations()

## -----------------------------------------------------------------------------
pattern <- "agenc"
kwr_test_regex(kwr, pattern)

## -----------------------------------------------------------------------------
kwr_add_pattern(
  pattern = pattern,
  recipe_file = recipe_file,
  recipe_type = "label",
  dim_name = "bussiness_type",
  value = "agency"
)
kwr <- kwr |> 
  kwr_classify(recipe_file)

## -----------------------------------------------------------------------------
kwr |> 
  kwr_queries()

## -----------------------------------------------------------------------------
kwr_add_pattern("compan", recipe_file, "label", "bussiness_type", "company")
kwr_add_pattern("consult", recipe_file, "label", "bussiness_type", "consultancy")
kwr_add_pattern("london", recipe_file, "label", "place")

kwr <- kwr |> 
  kwr_classify(recipe_file)

## -----------------------------------------------------------------------------
kwr |> 
  kwr_queries() |> 
  select(1:5)

## -----------------------------------------------------------------------------
kwr |> kwr_summary()

## -----------------------------------------------------------------------------
kwr |> kwr_dimension_table(bussiness_type)

## -----------------------------------------------------------------------------
kwr |> kwr_classified_queries()

## -----------------------------------------------------------------------------
kwr |> kwr_unclassified_queries()

## -----------------------------------------------------------------------------
kwr |> kwr_removed_queries()

## ----include=FALSE------------------------------------------------------------
file.remove(recipe_file)

