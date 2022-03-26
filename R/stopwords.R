#' Return stopwords as a character vector
#'
#' @param lang Language abbreviation. Only Czech (cs) is implemented now.
#'
#' @return Character vector of stopwords.
#' @export
kwr_stopwords <- function(lang = "cs") {
  checkmate::assert_true(lang == "cs")

  # converted to \unnnn by https://r12a.github.io/app-conversion/
  c(
    "a", "v", "se", "na", "je", "\u017Ee", "o", "s", "z", "do", "i", "to", "k",
    "ve", "pro", "za", "by", "ale", "si", "po", "jako", "podle", "od", "jsem",
    "tak", "jsou", "kter\u00E9", "ktere", "kter\u00FD", "ktery", "jeho",
    "v\u0161ak", "vsak", "bude", "nebo", "u\u017E", "uz", "jen", "byl", "jak",
    "u", "co", "p\u0159i", "pri", "a\u017E", "az", "aby", "m\u00E1", "ma",
    "kdy\u017E", "kdyz", "ne\u017E", "nez", "ze", "kter\u00E1", "ktera",
    "p\u0159ed", "pred", "b\u00FDt", "tak\u00E9", "bylo", "jsme", "nen\u00ED",
    "neni", "jejich", "je\u0161t\u011B", "jeste", "ani", "mezi", "byla",
    "sv\u00E9", "sve", "ji\u017E", "jiz", "pak", "kte\u0159\u00ED", "kteri",
    "proti", "t\u00EDm", "tim", "m\u016F\u017Ee", "muze", "tom", "kde",
    "\u010Di", "ci", "tedy", "pouze"
  )
}

#' Removes stopwords from a dataset, e.g. n-grams
#'
#' @param df A data frame with words or n-grams in the 1st column.
#' @param stopwords A vector of words (or n-grams) to be removed. It can be the
#'   output of the kwr_stopwords function.
#'
#' @return The same data set as df with the rows matching stopwords removed.
#' @export
kwr_remove_stopwords <- function(df, stopwords = kwr_stopwords()) {
  df |> dplyr::filter(dplyr::if_any(1, ~ !(. %in% stopwords)))
}
