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

  recipes <- read_recipes(recipe_file) |>
    purrr::keep(~ .x$type %in% c("remove", "include"))

  if (kwr$status == "pruned") {
    dataset <- kwr$pruned_data
  } else {
    dataset <- kwr$clean_data
  }

  pruned <- recipes |>
    purrr::reduce(process_recipe, .init = dataset, quiet = quiet)

  kwr$pruned_data <- pruned
  kwr$status <- "pruned"
  kwr
}
