## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(yaml)
library(keywordr)
library(tibble)
library(dplyr)

print_yaml <- function(x) {
  yaml <- yaml::as.yaml(x)
  cat("\n\n```{yaml}\n", yaml, "```\n\n", sep = "")
}

## ----echo=FALSE---------------------------------------------------------------
queries <- tibble(
  query = c("outdoor shoes", "cheap shoes", "luxury shoes", "white shirt", "blue trousers", "black hat"),
  volume = 10
)
queries

## ----include=FALSE------------------------------------------------------------
recipe_file <- file.path(tempdir(), "recipes.yml")

recipes <- yaml.load("
- type: label
  name: product
  rules:
  - match:
    - shoes
    - trousers
  values:
  - value: shirt
    rules:
    - match: shirts?
  - value: hat
    rules:
    - match: hats?
- type: label
  name: price
  rules:
  - match:
    - cheap
    - luxury
") |> 
  write_yaml(recipe_file)

kwr <- kwresearch(queries) |> 
  kwr_classify(recipe_file)

file.remove(recipe_file)

## ----echo=FALSE---------------------------------------------------------------
kwr |> 
  kwr_queries() |> 
  select(query_normalized:volume)

## -----------------------------------------------------------------------------
recipe_file <- file.path(tempdir(), "recipes.yml")
kwr_add_pattern(
  pattern = "shoes", 
  recipe_file = recipe_file, 
  recipe_type = "label", 
  dim_name = "product"
)
kwr_add_pattern(
  pattern = "trousers", 
  recipe_file = recipe_file, 
  recipe_type = "label", 
  dim_name = "product"
)
kwr_add_pattern(
  pattern = "shirts?", 
  recipe_file = recipe_file, 
  recipe_type = "label", 
  dim_name = "product", 
  value = "shirt"
)
kwr_add_pattern(
  pattern = "hats?", 
  recipe_file = recipe_file, 
  recipe_type = "label", 
  dim_name = "product", 
  value = "hat"
)
kwr_add_pattern(
  pattern = "cheap", 
  recipe_file = recipe_file, 
  recipe_type = "label", 
  dim_name = "price"
)
kwr_add_pattern(
  pattern = "luxury", 
  recipe_file = recipe_file, 
  recipe_type = "label", 
  dim_name = "price"
)
writeLines(readLines(recipe_file))

## ----include=FALSE------------------------------------------------------------
file.remove(recipe_file)

