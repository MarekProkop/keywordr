% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classify.R
\name{process_recipe}
\alias{process_recipe}
\title{Processes a single recipe}
\usage{
process_recipe(df, recipe, quiet = FALSE)
}
\arguments{
\item{df}{A data frame from a kwr object (either clean_data, or
classified_data).}

\item{recipe}{A single classification recipe.}

\item{quiet}{If TRUE prints no messages.}
}
\value{
A data frame with updated classification.
}
\description{
Processes a single recipe
}
\keyword{internal}
