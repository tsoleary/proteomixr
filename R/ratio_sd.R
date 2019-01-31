#' ratio_sd
#'
#' Taylor Expansion to get the sd of the ratio of two means
#' @param dat a data frame with peptide level data as rows and samples in columns
#' @param ratio
#' @param sd1
#' @param med1
#' @param sd2
#' @param med2
#' @keywords group
#' @export
#' @examples
#' ratio_sd(df, "ratio", "group1_sd", "group1_med", "ctrl_sd", "ctrl_med")

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
