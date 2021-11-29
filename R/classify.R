#' Classifies queries based on rules in a YAML file
#'
#' @param kwr A kwr object containg queries to be classified.
#' @param recipe_file A path to a YAML file with classification rules.
#' @param quiet If TRUE prints no messgaes.
#'
#' @return A kwr object where the queries are classified. Existing
#' classification is preserved or updated, new is added.
#' @export
#'
#' @examples
#' \dontrun{
#' kwr <- kwr |> kwr_classify("recipe.yml")
#' }
kwr_classify <- function(kwr, recipe_file, quiet = FALSE) {
  if (kwr$status == "classified") {
    dataset <- kwr$classified_data
  } else {
    dataset <- kwr$clean_data
  }
  recipes <- read_recipes(recipe_file)
  classified <- recipes |>
    purrr::reduce(process_recipe, .init = dataset, quiet) |>
    dplyr::relocate(.data$n_queries:.data$source, .after = dplyr::last_col())
  kwr$classified_data <- classified
  kwr$status <- "classified"
  kwr
}


# Private functions -------------------------------------------------------

#' Reads classification recipes from a YAML file
#'
#' @param path Path to YAML file.
#'
#' @return Classification recipes (a list).
#' @export
read_recipes <- function(path) {
  yaml::yaml.load_file(path)
}

#' Processes a single recipe
#'
#' @param df A data frame from a kwr object (either clean_data, or
#' classified_data).
#' @param recipe A single classification recipe.
#' @param quiet If TRUE prints no messgaes.
#'
#' @return A data frame with updated classification.
process_recipe <- function(df, recipe, quiet = FALSE) {
  if (recipe$type == "flag") {
    if (quiet) {
      message("Flag: ", recipe$name)
    }
    df |>
      set_flag(recipe$name, join_patterns(recipe$patterns), recipe$negate)
  } else if (recipe$type == "label") {
    if (quiet) {
      message("Label:", recipe$name)
    }
    if (is.null(recipe$values)) {
      df |> set_label(recipe$name, join_patterns(recipe$patterns))
    } else {
      recipe$values |> purrr::reduce(
        process_value, name = recipe$name, .init = df, quiet
      )
    }
  } else {
    stop(paste("Unknown recipe type", recipe$type))
  }
}

#' Processes a single value of a recipe of the type label
#'
#' @param df A data frame from a kwr object (either clean_data, or
#' classified_data).
#' @param value A label value (character).
#' @param name A label name (character).
#' @param quiet If TRUE prints no messgaes.
#'
#' @return A data frame with updated classification.
process_value <- function(df, value, name, quiet = TRUE) {
  if (quiet) {
    message("  Value: ", value$value)
  }
  df |>
    set_label(name, join_patterns(value$patterns), value = value$value)
}

#' Deduplicates regex patterns and joins them by "|"
#'
#' @param patterns A character vector of regex patterns.
#'
#' @return A character vector of length 1.
join_patterns <- function(patterns) {
  patterns |> unique() |> stringr::str_c(collapse = "|")
}

#' Sets a classification flag (logical value)
#'
#' @param df A data frame from a kwr object (either clean_data, or
#' classified_data).
#' @param name A name of the flag.
#' @param pattern A regex pattern (character vector of length 1).
#' @param negate If TRUE, queries that don't match the pattern will be flagged
#' as TRUE.
#'
#' @return A data frame with the flag updated.
set_flag <- function(df, name, pattern, negate = FALSE) {
  df[[name]] <- stringr::str_detect(
    df$query_normalized, pattern, negate = negate
  )
  df
}

#' Sets a classification label (character)
#'
#' @param df A data frame from a kwr object (either clean_data, or
#' classified_data).
#' @param name A name of the label.
#' @param pattern A regex pattern (character vector of length 1).
#' @param value An optional value of the label (character).
#'
#' @return A data frame with the label updated.
set_label <- function(df, name, pattern, value = NULL) {
  if (is.null(value)) {
    df[[name]] <- extract_pattern(df$query_normalized, pattern)
  } else {
    if (!name %in% names(df)) {
      df[[name]] <- NA_character_
    }
    df[[name]] <- dplyr::if_else(
      stringr::str_detect(df$query_normalized, pattern),
      join_labels(df[[name]], value),
      df[[name]]
    )
  }
  df
}

#' Deduplicates and joins multiple label values
#'
#' @param x A character vector.
#' @param y A character vector.
#' @param sep A character vector of length 1.
#'
#' @return Character vector of the same length as x. Unique label values
#' of x and y joined together.
join_labels <- function(x, y, sep = ",") {
  purrr::map2_chr(x, y, function(x, y) {
    if (is.na(x)) {
      y
    } else {
      stringr::str_c(
        unique(c(stringr::str_split(x, stringr::fixed(","))[[1]], y)),
        collapse = ","
      )
    }
  })
}

#' Extracts a regex pattern from a character vector (vectorized)
#'
#' @param x A character vector to extract pattern from.
#' @param pattern A character vector.
#'
#' @return A character vector of the same lenght as x.
extract_pattern <- function(x, pattern) {
  result <- purrr::map_chr(
    purrr::map(stringr::str_extract_all(x, pattern), unique),
    stringr::str_c, collapse = ","
  )
  result <- dplyr::if_else(result == "", NA_character_, result)
  return(result)
}
