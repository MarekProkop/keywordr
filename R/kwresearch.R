#' @title Creates an object of the kwresearch class
#'
#' @description An object of the kwresearch class is a home to your keyword
#'   research. You can import datasets into it, research queries, classify them
#'   and export the final keyword analysis.
#'
#' @param queries Queries to import as a data frame with at least two columns:
#'   \emph{query} (string) and \emph{volume} (numeric). Optionally the data
#'   frame can contain three additional columns: \emph{cpc} (double),
#'   \emph{input} (char) and \emph{source} (char). If you don't provide this
#'   argument, you can import queries later with \code{\link{kwr_import}} or
#'   \code{\link{kwr_import_mm}}.
#' @param accentize The import functions tries to add correct accents (diacritic
#'   marks) to queries without them.
#' @param normalize The import functions tries to unite queries, which differ
#'   only by order of words.
#'
#' @return An object of the kwresearch class.
#' @export
#'
#' @examples
#' # create an empty object
#'
#' kwr <- kwresearch()
#'
#' # create an object and import queries
#'
#' queries <- data.frame(
#'   query = c("seo", "keyword research"),
#'   volume = c(1000, 500)
#' )
#' kwr <- kwresearch(queries)
kwresearch <- function(queries = NULL, accentize = TRUE, normalize = TRUE) {
  checkmate::assert(
    checkmate::check_null(queries),
    checkmate::check_data_frame(queries)
  )
  checkmate::assert_flag(accentize)
  checkmate::assert_flag(normalize)

  result <- structure(
    list(
      status = "empty",
      options = list(
        accentize = accentize,
        normalize = normalize
      )
    ),
    class = "kwresearch"
  )
  if (!is.null(queries)) {
    result <- result |>
      kwr_import(queries)
  }
  result
}

#' Imports queries to a kwresearch object
#'
#' @param kwr An empty \code{\link{kwresearch}} object.
#' @param queries A data frame with at least one column \emph{query} and
#'   possibly with additional columns \emph{volume}, \emph{cpc}, \emph{input}
#'   and \emph{source}.
#'
#' @return The provided kwresearch object with imported queries.
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c("seo", "keyword research"),
#'   volume = c(1000, 500)
#' )
#' kwr <- kwresearch() |>
#'   kwr_import(queries)
kwr_import <- function(kwr, queries) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert_choice(kwr$status, "empty")
  checkmate::assert_data_frame(queries)

  empty_df <- tibble::tibble(
    query = character(),
    volume = integer(),
    cpc = double(),
    input = character(),
    source = character()
  )
  kwr$source_data <- queries |>
    dplyr::bind_rows(empty_df) |>
    dplyr::group_by(.data$query, .data$input, .data$source) |>
    dplyr::summarise(
      volume = dplyr::last(.data$volume),
      cpc = dplyr::last(.data$cpc),
      .groups = "drop"
    )
  kwr$clean_data <- clean_source_data(
    kwr$source_data, kwr$options$accentize, kwr$options$normalize
  )
  kwr$status <- "data"
  kwr
}


#' Outputs raw, source queries
#'
#' @param kwr A \code{\link{kwresearch}} object.
#' @param q An optional regular expression to filter the output queries.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c("seo", "keyword research"),
#'   volume = c(1000, 500)
#' )
#' kwr <- kwresearch(queries)
#' kwr_source_queries(kwr)
kwr_source_queries <- function(kwr, q = NULL) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert_choice(kwr$status, c("data", "pruned", "classified"))

  if (is.null(q)) {
    kwr$source_data
  } else {
    kwr$source_data |>
      dplyr::filter(stringr::str_detect(.data$query, q))
  }
}

#' Outputs cleaned (normalized and accentized) queries
#'
#' @param kwr A \code{\link{kwresearch}} object.
#'
#' @return A tibble.
#' @param q An optional regular expression to filter the output queries.
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c("seo", "keyword research"),
#'   volume = c(1000, 500)
#' )
#' kwr <- kwresearch(queries)
#' kwr_clean_queries(kwr)
kwr_clean_queries <- function(kwr, q = NULL) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert_choice(kwr$status, c("data", "pruned", "classified"))

  if (is.null(q)) {
    kwr$clean_data
  } else {
    kwr$clean_data |>
      dplyr::filter(stringr::str_detect(.data$query_normalized, q))
  }
}

#' Outputs pruned queries
#'
#' @param kwr A \code{\link{kwresearch}} object.
#' @param q An optional regular expression to filter the output queries.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c("seo", "keyword research", "nonsense"),
#'   volume = c(1000, 500, 10)
#' )
#' kwr <- kwresearch(queries)
#'
#' recipe_file <- file.path(tempdir(), "my-recipes.yml")
#' kwr_add_pattern(
#'   pattern = "nonsense",
#'   recipe_file = recipe_file,
#'   recipe_type = "remove"
#' )
#' kwr <- kwr_prune(kwr, recipe_file)
#' kwr_pruned_queries(kwr)
#'
#' file.remove(recipe_file)
kwr_pruned_queries <- function(kwr, q = NULL) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert_choice(kwr$status, c("pruned", "classified"))

  if (is.null(q)) {
    kwr$pruned_data
  } else {
    kwr$pruned_data |>
      dplyr::filter(stringr::str_detect(.data$query_normalized, q))
  }
}

#' Outputs classified queries
#'
#' @param kwr A \code{\link{kwresearch}} object.
#' @param q An optional regular expression to filter the output queries.
#'
#' @return A tibble.
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
#' kwr_classified_queries(kwr)
#'
#' file.remove(recipe_file)
kwr_classified_queries <- function(kwr, q = NULL) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert_true(kwr$status == "classified")

  if (is.null(q)) {
    kwr$classified_data
  } else {
    kwr$classified_data |>
      dplyr::filter(stringr::str_detect(.data$query_normalized, q))
  }
}

#' Outputs the most processed queries
#'
#' @param kwr A \code{\link{kwresearch}} object.
#' @param q An optional regular expression to filter the output queries.
#'
#' @return A tibble with queries.
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
kwr_queries <- function(kwr, q = NULL) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert_choice(kwr$status, c("data", "pruned", "classified"))

  switch(kwr$status,
    data = kwr_clean_queries(kwr, q),
    pruned = kwr_pruned_queries(kwr, q),
    classified = kwr_classified_queries(kwr, q)
  )
}

#' List queries that are not classified in all or specified dimensions
#'
#' @param kwr A \code{\link{kwresearch}} object.
#' @param label An optional dimension names as a character vector. If specified,
#'   classification in only those dimensions is considered.
#' @param q An optional regular expression to filter the output queries.
#'
#' @return A tibble with queries.
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
#' kwr_unclassified_queries(kwr)
#'
#' file.remove(recipe_file)
kwr_unclassified_queries <- function(kwr, label = NULL, q = NULL) {
  checkmate::assert_class(kwr, "kwresearch")
  df <- kwr |> kwr_classified_queries(q)
  dims <- kwr_dimension_names(df)
  if (is.null(label)) {
    df |>
      dplyr::filter(dplyr::if_all({{ dims }}, is_unclassified)) |>
      dplyr::select("query_normalized", "n_queries":"source")
  } else {
    df |>
      dplyr::filter(dplyr::across({{ label }}, is_unclassified)) |>
      dplyr::select(!{{ label }})
  }
}


#' Outputs queries removed by kwr_prune
#'
#' @param kwr A \code{\link{kwresearch}} object.
#' @param q An optional regular expression to filter the output queries.
#'
#' @return A tibble with queries removed by \code{\link{kwr_prune}}.
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c("seo", "keyword research", "nonsense"),
#'   volume = c(1000, 500, 10)
#' )
#' kwr <- kwresearch(queries)
#'
#' recipe_file <- file.path(tempdir(), "my-recipes.yml")
#' kwr_add_pattern(
#'   pattern = "nonsense",
#'   recipe_file = recipe_file,
#'   recipe_type = "remove"
#' )
#' kwr <- kwr_prune(kwr, recipe_file)
#' kwr_removed_queries(kwr)
#'
#' file.remove(recipe_file)
kwr_removed_queries <- function(kwr, q = NULL) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert_choice(kwr$status, c("pruned", "classified"))

  result <- kwr |>
    kwr_clean_queries() |>
    dplyr::anti_join(kwr_pruned_queries(kwr), by = "query_normalized")

  if (is.null(q)) {
    result
  } else {
    result |>
      dplyr::filter(stringr::str_detect(.data$query_normalized, q))
  }
}

#' Set a stopword list to use with n-gram functions
#'
#' @param kwr A \code{\link{kwresearch}} object.
#' @param stopwords Stopwords as a character vector. You can use the
#'   kwr_stopwords() function for Czech stopwords. These words will be removed
#'   from unigram listings. If NULL (default), no stopwords are used.
#'
#' @return A kwresearch object with stopwords set.
#' @export
kwr_use_stopwords <- function(kwr, stopwords = NULL) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert(
    checkmate::check_null(stopwords),
    checkmate::check_character(stopwords)
  )

  kwr$stopwords <- stopwords
  kwr
}

#' Prints number of queries in all phases of processing
#'
#' @param kwr A \code{\link{kwresearch}} object.
#'
#' @return None.
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c("seo", "keyword research", "nonsense"),
#'   volume = c(1000, 500, 10)
#' )
#' kwr <- kwresearch(queries)
#' kwr_summary(kwr)
kwr_summary <- function(kwr) {
  checkmate::assert_class(kwr, "kwresearch")

  if (kwr$status == "empty") {
    cat("Object is empty")
  } else {
    cat("Distinct input queries: ", dplyr::n_distinct(kwr_source_queries(kwr)), "\n")
    cat("Normalized queries:     ", nrow(kwr_clean_queries(kwr)), "\n")
    if (kwr$status %in% c("pruned", "classified")) {
      pruned_queries <- nrow(kwr_pruned_queries(kwr))
      cat("Pruned queries:         ", pruned_queries, "\n")
    }
    if (kwr$status == "classified") {
      unclassified_queries <- nrow(kwr_unclassified_queries(kwr))
      classified_queries <- pruned_queries - unclassified_queries
      cat("Classified queries:     ", classified_queries, "\n")
      cat("Unclassified queries:   ", unclassified_queries, "\n")
    }
  }

}

#' Outputs frequency table of a given dimension
#'
#' @param x A \code{\link{kwresearch}} object, whose queries are already
#'   classified, or a data frame of classified queries (e.g. the result of the
#'   \code{\link{kwr_classified_queries}} function).
#' @param column A dimension (column) as a name, not a string.
#'
#' @return A tibble.
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
#' kwr_dimension_table(kwr, my_label)
#'
#' file.remove(recipe_file)
kwr_dimension_table <- function(x, column) {
  checkmate::assert(
    checkmate::check_class(x, "kwresearch"),
    checkmate::check_class(x, "data.frame")
  )

  if (inherits(x, "kwresearch")) {
    checkmate::assert_true(x$status == "classified")
    df <- x |> kwr_classified_queries()
  } else {
    df <- x
  }

  df |>
    tidyr::separate_rows({{ column }}, sep = ",") |>
    dplyr::group_by({{ column }}) |>
    dplyr::summarise(
      n = dplyr::n(),
      volume = sum(.data$volume)
    ) |>
    dplyr::arrange(dplyr::desc(.data$n))
}

#' Gets the names of all dimensions
#'
#' @param df A data frame to get dimension names from.
#'
#' @return A vector of names.
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
#' kwr |>
#'   kwr_queries() |>
#'   kwr_dimension_names()
#'
#' file.remove(recipe_file)
kwr_dimension_names <- function(df) {
  checkmate::check_class(df, "data.frame")
  df |>
    names() |>
    setdiff(c(
      "query_normalized", "n_queries", "volume", "cpc", "query_original", "input", "source"
    ))
}


# Private functions -------------------------------------------------------


clean_source_data <- function(source_data, accentize, normalize) {
  if (accentize) {
    result <- source_data |>
      dplyr::mutate(
        query_normalized = accentize_queries(.data$query, .data$volume),
        .after = "query"
      )
  } else {
    result <- source_data |>
      dplyr::mutate(query_normalized = .data$query, .after = "query")
  }
  if (normalize) {
    result <- result |>
      dplyr::mutate(
        query_normalized = normalize_queries(
          .data$query_normalized, .data$volume
        )
      )
  }
  result |>
    aggregate_clean_data()
}

#' Tries to add accents or diacritics where they should be
#'
#' @param x Character vector of queries.
#' @param vol Character vector of search volumes corresponding to queries in x.
#'
#' @return Character vector of all queries in x, with accents added to some
#' of them.
#' @importFrom rlang .data
#' @keywords internal
accentize_queries <- function(x, vol) {
  tibble::tibble(
    x,
    ascii = stringi::stri_trans_general(x, "Latin-ASCII"),
    dist = vol * ((stringdist::stringdist(x, .data$ascii) + 1) ** 3),
    r = seq_len(length(x))
  ) |>
    dplyr::arrange(.data$ascii, dplyr::desc(.data$dist), dplyr::desc(vol)) |>
    dplyr::group_by(.data$ascii) |>
    dplyr::mutate(accentized = dplyr::first(x)) |>
    dplyr::arrange(.data$r) |>
    dplyr::pull(.data$accentized)
}

normalize_queries <- function(x, value) {
  tibble::tibble(
    x,
    value,
    sorted = stringr::str_split(x, stringr::boundary("word")) |>
      purrr::map_chr(~ stringr::str_c(stringr::str_sort(.x), collapse = " "))
  ) |>
    dplyr::group_by(.data$sorted) |>
    dplyr::mutate(normalized = x[which.max(.data$value)]) |>
    dplyr::pull(.data$normalized)
}

aggregate_clean_data <- function(mm_data) {
  mm_data |>
    dplyr::group_by(.data$query, .data$query_normalized, .data$source) |>
    dplyr::summarise(
      volume = dplyr::last(.data$volume),
      cpc = dplyr::last(.data$cpc),
      input = stringr::str_c(unique(.data$input), collapse = ","),
      .groups = "drop"
    ) |>
    dplyr::relocate("source", .after = "input") |>
    dplyr::group_by(.data$query_normalized) |>
    dplyr::summarise(
      n_queries = dplyr::n_distinct(.data$query),
      cpc = stats::weighted.mean(.data$cpc, .data$volume),
      volume = sum(.data$volume),
      query_original = stringr::str_c(unique(.data$query), collapse = ","),
      input = dplyr::first(.data$input),
      source = stringr::str_c(unique(.data$source), collapse = ",")
    ) |>
    dplyr::relocate("cpc", .after = "volume") |>
    dplyr::arrange(dplyr::desc(.data$volume), .data$query_normalized)
}

is_unclassified <- function(x) {
  if (is.character(x)) {
    is.na(x)
  } else if (is.logical(x)) {
    !x
  } else {
    stop("A dimension must be either character, or logical vector.")
  }
}
