% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recipes.R
\name{kwr_add_pattern}
\alias{kwr_add_pattern}
\title{Adds a new regex pattern to a pruning or classification recipe}
\usage{
kwr_add_pattern(
  pattern = NULL,
  recipe_file = NULL,
  recipe_type = NULL,
  dim_name = NULL,
  value = NULL
)
}
\arguments{
\item{pattern}{A regular expression.}

\item{recipe_file}{A YAML file, which the pattern should be saved in.}

\item{recipe_type}{Recipe type. One of: 'remove', 'include', 'tag' or
'label'.}

\item{dim_name}{Dimension name for recipe types 'tag' and 'label'.}

\item{value}{Optional value for recipe types 'tag' and 'label'.}
}
\value{
Nothing.
}
\description{
If the \code{recipe_file} exists, it is loaded and the \code{pattern} is inserted into
it. If the \code{recipe_file} does not exist, it is created.
}
\examples{
recipe_file <- file.path(tempdir(), "my-recipes.yml")

# Pruning recipe:
kwr_add_pattern(
  pattern = "xyz",
  recipe_file = recipe_file,
  recipe_type = "remove"
)

# Label recipe:
kwr_add_pattern(
  pattern = "xyz",
  recipe_file = recipe_file,
  recipe_type = "label",
  dim_name = "my_label"
)

file.remove(recipe_file)
}
