#' Adds new classification recipes to a kwresearch object
#'
#' @param kwr A kwresearch object.
#' @param recipe_file A path to a recipe file in YAML format.
#'
#' @return The input kwresearch object with updated classification recipes. If
#'   the kwresearch object already contained recipes, they are merged with the
#'   new ones.
#' @export
kwr_use_recipes <- function(kwr, recipe_file) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert_file_exists(recipe_file, access = "r", extension = "yml")

  new_recipes <- read_recipes(recipe_file)
  if (is.null(kwr$recipes)) {
    kwr$recipes <- new_recipes
  } else {
    kwr$recipes <- c(kwr$recipes, new_recipes)
  }
  kwr
}

#' Classifies queries based on recipes
#'
#' @param kwr A kwr object containg queries to be classified, and classification
#'   recipes.
#' @param quiet If TRUE prints no messgaes.
#'
#' @return A kwr object where the queries are classified. Existing
#'   classification is preserved or updated, new is added.
#' @export
#'
#' @examples
#' \dontrun{
#' kwr <- kwr |> kwr_classify()
#' }
kwr_classify <- function(kwr, quiet = FALSE) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert_choice(kwr$status, c("data", "pruned", "classified"))
  checkmate::assert_list(kwr$recipes, min.len = 1)
  checkmate::assert_flag(quiet)

  if (kwr$status == "classified") {
    dataset <- kwr$classified_data
  } else if (kwr$status == "pruned") {
    dataset <- kwr$pruned_data
  } else {
    dataset <- kwr$clean_data
  }
  classified <- kwr$recipes |>
    purrr::keep(~ .x$type %ïn% c("flag", "label")) |>
    purrr::reduce(process_recipe, .init = dataset, quiet = quiet) |>
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
#' @keywords internal
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
#' @keywords internal
process_recipe <- function(df, recipe, quiet = FALSE) {
  switch (recipe$type,
    remove = {
      if (!quiet) {
        message("Removing queries...")
      }
      recipe$rules |>
        purrr::reduce(process_remove_rule, .init = df)
    },
    flag = {
      if (!quiet) {
        message("Flag: ", recipe$name)
      }
      df |>
        set_flag(recipe$name, join_patterns(recipe$patterns), recipe$negate)
    },
    label = {
      if (!quiet) {
        message("Label:", recipe$name)
      }
      if (!is.null(recipe$patterns)) {
        df <- df |> set_label(recipe$name, join_patterns(recipe$patterns))
      }
      if (!is.null(recipe$values)) {
        df <- recipe$values |> purrr::reduce(
          process_value, name = recipe$name, .init = df, quiet
        )
      }
      df
    },
    {
      stop(paste("Unknown recipe type", recipe$type))
    }
  )
}

process_remove_rule <- function(df, rule) {
  if (is.null(rule$except)) {
    df |>
      dplyr::filter(
        !stringr::str_detect(query_normalized, join_patterns(rule$match))
      )
  } else {
    df |>
      dplyr::filter(
        !stringr::str_detect(query_normalized, join_patterns(rule$match)) |
        stringr::str_detect(query_normalized, join_patterns(rule$except))
      )
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
#' @keywords internal
process_value <- function(df, value, name, quiet = FALSE) {
  if (!quiet) {
    message("  Value: ", value$value)
  }
  df |>
    set_label(
      name = name,
      pattern = join_patterns(value$patterns),
      value = value$value,
      exclude = join_patterns(value$exclude)
    )
}

#' Deduplicates regex patterns and joins them by "|"
#'
#' @param patterns A character vector of regex patterns.
#'
#' @return A character vector of length 1.
#' @keywords internal
join_patterns <- function(patterns) {
  if (is.null(patterns)) return(patterns)
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
#' @keywords internal
set_flag <- function(df, name, pattern, negate = FALSE) {
  df[[name]] <- stringr::str_detect(
    df$query_normalized, pattern, negate = negate
  )
  df
}

#' Sets a classification label (character)
#'
#' @param df A data frame from a kwr object (either clean_data, or
#'   classified_data).
#' @param name A name of the label.
#' @param pattern A regex pattern (character vector of length 1). The label is
#'   set to matched queries only.
#' @param value An optional value of the label (character).
#' @param exclude A regex pattern (character vector of lenth 1). The label is
#'   never set to matched queries.
#'
#' @return A data frame with the label updated.
#' @keywords internal
set_label <- function(df, name, pattern, value = NULL, exclude = NULL) {
  if (is.null(value)) {
    if (is.null(exclude)) {
      df[[name]] <- extract_pattern(df$query_normalized, pattern)
    } else {
      df[[name]] <- dplyr::if_else(
        !stringr::str_detect(df$query_normalized, exclude),
        extract_pattern(df$query_normalized, pattern),
        NA_character_
      )
    }
  } else {
    if (!name %in% names(df)) {
      df[[name]] <- NA_character_
    }
    if (is.null(exclude)) {
      df[[name]] <- dplyr::if_else(
        stringr::str_detect(df$query_normalized, pattern),
        join_labels(df[[name]], value),
        df[[name]]
      )
    } else {
      df[[name]] <- dplyr::if_else(
        stringr::str_detect(df$query_normalized, pattern) &
          !stringr::str_detect(df$query_normalized, exclude),
        join_labels(df[[name]], value),
        df[[name]]
      )
    }
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
#' @keywords internal
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
#' @keywords internal
extract_pattern <- function(x, pattern) {
  result <- purrr::map_chr(
    purrr::map(stringr::str_extract_all(x, pattern), unique),
    stringr::str_c, collapse = ","
  )
  result <- dplyr::if_else(result == "", NA_character_, result)
  return(result)
}
