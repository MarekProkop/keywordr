#' Removes unwanted queries according to YAML recipes
#'
#' If the status of the kwresearch object is 'data', it changes to 'pruned' and
#' pruned data is created from clean data. I pruned data already exists (ie.
#' status = 'pruned') then pruned data are pruned again.
#'
#' @param kwr A kwresearch object to be pruned.
#' @param recipe_file A path to a recipe file in YAML format.
#' @param quiet If TRUE prints no messgaes.
#'
#' @return A kwr object with pruned data and status = 'pruned'.
#' @export
#'
#' @examples
#' \dontrun{
#' kwr <- kwr |> kwr_prune("prune.yml")
#' }
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


# Private functions -------------------------------------------------------

#' Processes a single pruning recipe
#'
#' @param df A data frame from a kwr object (either clean_data, or
#' pruned_data).
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

