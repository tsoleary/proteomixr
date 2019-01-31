#' Count the degrees of freedom for each group
#'
#' Counts the number of observations within each group for each peptide and
#' divides by the number of replicates -- defaults to one replicate per sample.
#' @param dat a data frame containing samples and observations to be counted
#' @param col a numeric vector containing the column numbers of the group to be
#' counted
#' @param rep number of replicates for each sample defaults to one.
#' @keywords degrees of freedom
#' @export
#' @examples
#' # selects the columns containing a string that identifies the group
#' group1 <- grep("Sample_norm", colnames(data))
#' # three analytical replicates were run for each
#' data$group1_df <- count_df(data, group1, rep = 3)


count_df <- function (dat, col, rep = 1){
  list <- NULL
  for (i in 1:nrow(dat)){
    temp <- length(which(!is.na(dat[i, col])))
    temp <- temp / rep
    if(temp >= 1){
      temp <- temp - 1
    }
    list <- c(list, temp)
  }
  return(list)
}
