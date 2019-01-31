#' count_df
#'
#' Function to count degrees of freedom in a group, defaults without duplicates
#' @param dat a data frame with peptide level data as rows and samples in columns
#' @param col a numeric vector containing the column numbers of the group to be
#' counted
#' @keywords degrees of freedom
#' @export
#' @examples
#' count_df(df, c(4:13), dup = 3)


count_df <- function (dat, col, dup = 1){
  list <- NULL
  for (i in 1:nrow(dat)){
    temp <- length(which(!is.na(dat[i, col])))
    temp <- temp / dup
    if(temp >= 1){
      temp <- temp - 1
    }
    list <- c(list, temp)
  }
  return(list)
}
