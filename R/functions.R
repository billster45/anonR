#' anonCols
#' Anonymise the values in one more columns of a data frame
#' function from: \href{https://stackoverflow.com/a/10455661}{Stack Overflow}
#' @param df The data frame name
#' @param colIDs The location of the columns with values to anonymise. Use either column names or integer indices as shownin the two examples.
#' @keywords anonymise
#' @export
#' @examples
#' anonCols(datasets::sleep, c("group","ID"))
#' anonCols(datasets::sleep, c(2,3))
anonCols <- function(df, colIDs) {
  id <- if(base::is.character(colIDs)) base::match(colIDs, base::names(df)) else colIDs
  for(id in colIDs) {
    df[[id]] <- base::as.character(base::as.numeric(base::as.factor(df[[id]])))
  }
  df
}
