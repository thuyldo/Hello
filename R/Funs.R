#' Hello World
#'
#' `hello` says _"Hello"_ in the user-specified language. The user is asked to give her/his name so that the hello message gets personalized.
#'
#' @usage hello(who,lang="EN",LangData=language)
#'
#' @param
#' who a `character` vector of length 1 that specifies the name of the person
#'
#'@param
#' lang a `character` vector of length 1
#'
#'@param
#' LangData an optional data.frame with two colmns each of mode `character`.
#'
#' @return a `character` vector with a personalized _"hello"_ message.
#'
#' @examples
#'hello("James")
#'hello("Amélie","Es")
#'
#' @export

hello <- function(who, lang = "EN", LangData = lab4::language) {
  if (!exists("who", mode = "character") | length(who) > 1) {
    stop("Please enter a valid name; see ?hello")
  }

  LangData <- data.frame(LangData)

  if (ncol(LangData) > 2) {
    stop("Please enter a valid language data set; see ?hello")
  }

  colnames(LangData) <- c("code", "hello")

  if ((mode(LangData$code) != "character") | (mode(LangData$hello) != "character")) {
    stop("Please enter a valid language data set; see ?hello")
  }

  llang <- tolower(lang)

  hello <- subset(LangData, LangData$code == llang)[[2]]

  ifelse(
    length(hello) == 1,
    stringr::str_c(hello, ", ", who, "!", sep = ""),
    stringr::str_c("Sorry, ", who, ", ", "your language ", "('", lang, "') ", "is not available!", sep = "")
  ) |> cat()
}
