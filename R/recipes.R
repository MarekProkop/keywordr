#' Adds a new regex pattern to a pruning or classification recipe
#'
#' If the `recipe_file` exists, it is loaded and the `pattern` is inserted into
#' it. If the `recipe_file` does not exist, it is created.
#'
#' @param pattern A regular expression.
#' @param recipe_file A YAML file, which the pattern should be saved in.
#' @param recipe_type Recipe type. One of: 'remove', 'include', 'tag' or
#'   'label'.
#' @param dim_name Dimension name for recipe types 'tag' and 'label'.
#' @param value Optional value for recipe types 'tag' and 'label'.
#'
#' @return Nothing.
#' @export
#'
#' @examples
#' recipe_file <- file.path(tempdir(), "my-recipes.yml")
#'
#' # Pruning recipe:
#'
#' kwr_add_pattern(
#'   pattern = "xyz",
#'   recipe_file = recipe_file,
#'   recipe_type = "remove"
#' )
#'
#' # Label recipe:
#'
#' kwr_add_pattern(
#'   pattern = "xyz",
#'   recipe_file = recipe_file,
#'   recipe_type = "label",
#'   dim_name = "my_label"
#' )
#'
#' file.remove(recipe_file)
kwr_add_pattern <- function(pattern = NULL,
                            recipe_file = NULL,
                            recipe_type = NULL,
                            dim_name = NULL,
                            value = NULL) {
  checkmate::assert_string(pattern)
  checkmate::assert(
    checkmate::check_file_exists(recipe_file, access = "r", extension = "yml"),
    checkmate::check_path_for_output(recipe_file, overwrite = TRUE, extension = "yml")
  )
  checkmate::assert_choice(
    recipe_type, choices = c("remove", "include", "flag", "label")
  )
  if (checkmate::test_choice(recipe_type, choices = c("remove", "include"))) {
    checkmate::assert_null(dim_name)
    checkmate::assert_null(value)
  } else if (checkmate::test_choice(recipe_type, "label")) {
    checkmate::assert_string(dim_name)
    checkmate::assert(
      checkmate::check_null(value),
      checkmate::check_string(value)
    )
  } else {
    checkmate::assert_string(dim_name)
    checkmate::assert_null(value)
  }

  if (!is.null(value)) {
    value <- stringi::stri_enc_toutf8(value)
  }

  if (file.exists(recipe_file)) {
    all_recipes <- yaml::read_yaml(file = recipe_file)
    if (recipe_type == "remove") {
      recipe_index <- purrr::detect_index(all_recipes, ~ .$type == recipe_type)
      if (recipe_index == 0) {
        # create a new recipe
        all_recipes <- c(
          all_recipes,
          build_recipe(recipe_type, dim_name, value, pattern)
        )
      } else {
        # append to the existing recipe
        all_recipes[[recipe_index]]$rules[[1]]$match <- unique(c(
          all_recipes[[recipe_index]]$rules[[1]]$match, pattern
        ))
      }
    } else if (recipe_type == "label") {
      recipe_index <- purrr::detect_index(
        all_recipes,
        ~ utils::hasName(., "name") && .$name == dim_name
      )
      if (recipe_index == 0) {
        # create a new recipe
        all_recipes <- c(
          all_recipes,
          build_recipe(recipe_type, dim_name, value, pattern)
        )
      } else {
        if (is.null(value)) {
          # append to the existing recipe
          all_recipes[[recipe_index]]$rules[[1]]$match <- unique(c(
            all_recipes[[recipe_index]]$rules[[1]]$match, pattern
          ))
        } else {
          if (utils::hasName(all_recipes[[recipe_index]], "values")) {
            # values exist, find the right one
            value_index <- purrr::detect_index(
              all_recipes[[recipe_index]]$values,
              ~ utils::hasName(., "value") && .$value == value
            )
            if (value_index == 0) {
              # value does not exist, add it
              all_recipes[[recipe_index]]$values <- c(
                all_recipes[[recipe_index]]$values,
                list(list(value = value, rules = list(list(match = pattern))))
              )
            } else {
              # value exists, append pattern
              all_recipes[[recipe_index]]$values[[value_index]]$rules[[1]]$match <-
                unique(c(
                  all_recipes[[recipe_index]]$values[[value_index]]$rules[[1]]$match,
                  pattern
                ))
            }
          } else {
            # values does not exists, add them
            all_recipes[[recipe_index]]$values <- list(list(
              value = value,
              rules = list(list(match = pattern))
            ))
          }
        }
      }
    } else {
      stop("Recipe types 'include' or 'flag' are not supported yet. Please edit YAML recipes directly.")
    }
  } else {
    all_recipes <- build_recipe(recipe_type, dim_name, value, pattern)
  }

  # recipe_index <- all_recipes |> purrr::detect_index(~ .$name == dim_name)
  # if (recipe_index == 0) {
  # }

  all_recipes |> yaml::write_yaml(file = recipe_file)
}


# Private functions -------------------------------------------------------

build_recipe <- function(recipe_type, dim_name = NULL, value = NULL, pattern) {
  result <- list(type = recipe_type)

  if (!is.null(dim_name)) {
    result <- c(result, list(name = dim_name))
  }
  if (is.null(value)) {
    result <- c(result, list(rules = list(list(match = pattern))))
  } else {
    result <- c(
      result,
      list(values = list(list(value = value, rules = list(list(match = pattern)))))
    )
  }
  list(result)
}
