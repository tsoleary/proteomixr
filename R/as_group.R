#' as_group
#'
#' This function allows you to express your love of cats.
#' @param dat data frame with peptide level data as rows and samples in columns
#' @param col a numeric vector containing the column numbers of the group
#' that the function will be performed on
#' @keywords group
#' @export
#' @examples
#' as_group(df, c(4:6))

as_group <- function (dat, col, FUN = median){
  list <- NULL
  for (i in 1:nrow(dat)){
    temp <- FUN(as.numeric(dat[i, col], na.rm = TRUE))
    list <- c(list, temp)
  }
  return(list)
}
