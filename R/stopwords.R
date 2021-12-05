#' Return stopwords as a character vector
#'
#' @param lang Language abbreviation. Only Czech (cs) is implemented now.
#'
#' @return Character vector of stopwords.
#' @export
kwr_stopwords <- function(lang = "cs") {
  checkmate::assert_true(lang == "cs")

  c(
    "a", "v", "se", "na", "je", "že", "o", "s", "z", "do", "i", "to", "k",
    "ve", "pro", "za", "by", "ale", "si", "po", "jako", "podle", "od", "jsem",
    "tak", "jsou", "které", "ktere", "který", "ktery", "jeho", "však",
    "vsak", "bude", "nebo", "už", "uz", "jen", "byl", "jak", "u", "co",
    "při", "pri", "až", "az", "aby", "má", "ma", "když", "kdyz", "než",
    "nez", "ze", "která", "ktera", "před", "pred", "být", "také", "bylo",
    "jsme", "není", "neni", "jejich", "ještě", "jeste", "ani", "mezi",
    "byla", "své", "sve", "již", "jiz", "pak", "kteří", "kteri", "proti",
    "tím", "tim", "může", "muze", "tom", "kde", "či", "ci", "tedy", "pouze"
  )
}
