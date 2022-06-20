#' Classifies queries based on recipes in a YAML file
#'
#' @param kwr A \code{\link{kwresearch}} object containing queries to be
#'   classified.
#' @param recipe_file A path to a recipe file in YAML format.
#' @param quiet If \code{TRUE}, prints no messages.
#'
#' @return A kwresearch object in which the queries are classified according to
#'   the recipes in the provided YAML file. Any previous classification is
#'   preserved or updated.
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c("seo", "keyword research"),
#'   volume = c(1000, 500)
#' )
#' kwr <- kwresearch(queries)
#'
#' recipe_file <- file.path(tempdir(), "my-recipes.yml")
#' kwr_add_pattern(
#'   pattern = "seo",
#'   recipe_file = recipe_file,
#'   recipe_type = "label",
#'   dim_name = "my_label"
#' )
#' kwr <- kwr_classify(kwr, recipe_file)
#' kwr_queries(kwr)
#'
#' file.remove(recipe_file)
kwr_classify <- function(kwr, recipe_file = NULL, quiet = FALSE) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert_choice(kwr$status, c("data", "pruned", "classified"))
  checkmate::assert_file_exists(recipe_file, access = "r", extension = "yml")
#  checkmate::assert_list(kwr$recipes, min.len = 1)
  checkmate::assert_flag(quiet)

  if (kwr$status == "classified") {
    dataset <- kwr$classified_data
  } else if (kwr$status == "pruned") {
    dataset <- kwr$pruned_data
  } else {
    dataset <- kwr$clean_data
  }
  classified <- read_recipes(recipe_file) |>
    purrr::keep(~ .x$type %in% c("flag", "label")) |>
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
#' @param quiet If TRUE prints no messages.
#'
#' @return A data frame with updated classification.
#' @keywords internal
process_recipe <- function(df, recipe, quiet = FALSE) {
  switch(recipe$type,
    flag = {
      if (!quiet) {
        message("Flag: ", recipe$name)
      }
      df <- recipe$rules |> purrr::reduce(
        process_flag_rule, recipe$name, recipe$negate, .init = df
      )
    },
    label = {
      if (!quiet) {
        message("Label:", recipe$name)
      }
      if (!is.null(recipe$rules)) {
        df <- recipe$rules |> purrr::reduce(
          process_label_rule, recipe$name, .init = df
        )
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

#' Processes a single value of a recipe of the type label
#'
#' @param df A data frame from a kwr object (either clean_data, or
#' classified_data).
#' @param value A label value (character).
#' @param name A label name (character).
#' @param quiet If TRUE prints no messages.
#'
#' @return A data frame with updated classification.
#' @keywords internal
process_value <- function(df, value, name, quiet = FALSE) {
  if (!quiet) {
    message("  Value: ", value$value)
  }
  value$rules |> purrr::reduce(
    process_label_rule, name, value$value, .init = df
  )
}

process_label_rule <- function(df, rule, name, value = NULL) {
  df |>
    set_label(
      name = name,
      value = value,
      pattern = join_patterns(rule$match),
      exclude = join_patterns(rule$except)
    )
}

process_flag_rule <- function(df, rule, name, negate = FALSE) {
  df |>
    set_flag(
      name = name,
      pattern = join_patterns(rule$match),
      negate = negate
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
#' @param exclude A regex pattern (character vector of length 1). The label is
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
#' @param pattern A regex pattern. If it contains capture groups, they all will
#'   be extracted.
#'
#' @return A character vector of the same length as x.
#' @keywords internal
#' @importFrom stats na.omit
extract_pattern <- function(x, pattern) {
  collapse_matrix <- function(mat) {
    purrr::map_chr(seq_len(nrow(mat)), \(i) {
      vec <- na.omit(mat[i,])
      vec[min(length(vec), 2):length(vec)]
    }) |>
      unique() |>
      stringr::str_c(collapse = ",")
  }
  match_list <- stringr::str_match_all(x, pattern)
  match_list |>
    purrr::map_chr(collapse_matrix) |>
    (\(x) replace(x, x == "", NA))()
}
