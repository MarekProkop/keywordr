#' Removes unwanted queries according to YAML recipes
#'
#' If the status of the kwresearch object is 'data', it changes to 'pruned' and
#' pruned data is created from clean data. I pruned data already exists (i.e.
#' status = 'pruned') then pruned data are pruned again.
#'
#' @param kwr A \code{\link{kwresearch}} object to be pruned.
#' @param recipe_file A path to a recipe file in YAML format.
#' @param quiet If TRUE prints no messages.
#'
#' @return A kwresearch object with pruned data and status = 'pruned'.
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
#'
#' file.remove(recipe_file)
kwr_prune <- function(kwr, recipe_file, quiet = FALSE) {
  checkmate::assert_class(kwr, "kwresearch")
  checkmate::assert_choice(kwr$status, c("data", "pruned", "classified"))
  checkmate::assert_file_exists(recipe_file, access = "r", extension = "yml")

  if (!quiet) {
    start_time <- Sys.time()
    message("Removing queries...")
  }

  recipes <- read_recipes(recipe_file) |>
    purrr::keep(~ .x$type %in% c("remove", "include"))

  if (kwr$status == "pruned") {
    dataset <- kwr$pruned_data
  } else {
    dataset <- kwr$clean_data
  }
  n_queries <- nrow(dataset)

  pruned <- recipes |>
    purrr::reduce(process_prune_recipe, .init = dataset)

  kwr$pruned_data <- pruned
  kwr$status <- "pruned"

  if (!quiet) {
    message(stringr::str_glue(
      "Removed {n_queries - nrow(pruned)} queries out of {n_queries}. Duration: {round(Sys.time() - start_time, 3)}s"
    ))
  }

  kwr
}


#' Outputs queries longer than a specific number of characters
#'
#' Very long queries tend to be too specific and you may want to exclude them
#' from the analysis. You can use this function to review them before using the
#' \code{\link{kwr_prune_long_queries}} function to remove them.
#'
#' @param kwr A \code{\link{kwresearch}} object.
#' @param longer_than A number.
#'
#' @return The output of the \code{\link{kwr_queries}} function filtered for
#'   queries longer than `longer_than` characters.
#' @seealso \code{\link{kwr_prune_long_queries}}
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c(
#'     "seo",
#'     "this is a very long query made up of 67 characters including spaces"
#'   ),
#'   volume = c(1000, 5)
#' )
#' kwr <- kwresearch(queries)
#' kwr_long_queries(kwr, longer_than = 65)
kwr_long_queries <- function(kwr, longer_than = 60) {
  kwr |> kwr_queries() |>
    dplyr::filter(dplyr::if_any(1, ~ stringr::str_length(.) > longer_than)) |>
    dplyr::select(1, .data$volume)
}


#' From pruned data removes queries longer than a specific number of characters
#'
#' Very long queries tend to be too specific and you may want to exclude them
#' from the analysis. That's what this function is for.
#'
#' @param kwr A \code{\link{kwresearch}} object.
#' @param longer_than A number.
#'
#' @return A kwresearch object with pruned data and status = 'pruned'.
#' @seealso \code{\link{kwr_long_queries}}
#' @export
#'
#' @examples
#' queries <- data.frame(
#'   query = c(
#'     "seo",
#'     "this is a very long query made up of 67 characters including spaces"
#'   ),
#'   volume = c(1000, 5)
#' )
#' kwr <- kwresearch(queries)
#' kwr_long_queries(kwr, longer_than = 65)
#' kwr <- kwr_prune_long_queries(kwr, longer_than = 65)
kwr_prune_long_queries <- function(kwr, longer_than = 60) {
  if (kwr$status == "pruned") {
    dataset <- kwr$pruned_data
  }
  else {
    dataset <- kwr$clean_data
  }
  kwr$pruned_data <- dataset |>
    dplyr::filter(dplyr::if_any(1, ~ stringr::str_length(.) <= longer_than))
  kwr$status <- "pruned"
  kwr
}


# Private functions -------------------------------------------------------


#' Processes a single pruning recipe
#'
#' @param df A data frame from a \code{\link{kwresearch}} object (either
#'   clean_data, or pruned_data).
#' @param recipe A single pruning recipe (type 'remove').
#'
#' @return A data frame with pruned queries.
#' @keywords internal
process_prune_recipe <- function(df, recipe) {
  if (recipe$type == "remove") {
    recipe$rules |>
      purrr::reduce(process_remove_rule, .init = df)
  } else {
    stop(stringr::str_glue(
      "Recipe type '{recipe$type}' is not implemented for pruning."
    ))
  }
}

process_remove_rule <- function(df, rule) {
  if (is.null(rule$except)) {
    df |>
      dplyr::filter(
        !stringr::str_detect(.data$query_normalized, join_patterns(rule$match))
      )
  } else {
    df |>
      dplyr::filter(
        !stringr::str_detect(.data$query_normalized, join_patterns(rule$match)) |
          stringr::str_detect(.data$query_normalized, join_patterns(rule$except))
      )
  }
}

