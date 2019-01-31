#' Standard deviation of a ratio of two means
#'
#' creates a vector with the sd of the ratio for each peptide using a
#' Taylor Expansion to get the sd of the ratio of two means
#' @param dat a data frame with peptide level data as rows and samples
#' in columns
#' @param ratio character string of ratio column name
#' @param sd1 character string of sd of group1 column name
#' @param med1 character string of average of group1 column name
#' @param sd2 character string of sd of group2 column name
#' @param med2 character string of average of group2 column name
#' @keywords standard deviation of ratio, taylor expansion
#' @export
#' @examples
#' df$ratio_sd <- ratio_sd(df, "ratio", "group1_sd",
#'                         "group1_med", "ctrl_sd", "ctrl_med")

ratio_sd <- function (dat, ratio, sd1, med1, sd2, med2){
  ratio_sd_pep <- NULL
  for (i in 1:nrow(dat)){
    temp1 <- (dat[i, sd1] / dat[i, med1])^2
    temp2 <- (dat[i, sd2] / dat[i, med2])^2
    result <- dat[i, ratio] * sqrt(temp1 + temp2)
    ratio_sd_pep <- c(ratio_sd_pep, result)
  }
  return(ratio_sd_pep)
}
