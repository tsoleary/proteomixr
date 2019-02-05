#' P-value of t-test between two groups for each protein group
#'
#' This function returns the a data frame containing the p-value for each
#' protein grouping in a two-sided non-paired unequal variance t-test
#' between two specified groups
#' @param dat a data frame stacked into separate columns with the natural log
#' of the normalized abundance for each group with a column containing the
#' Master.Protein.Accessions
#' @param group character string of the first group
#' @param ctrl character string of the second (control) group
#' @param col character string of the column with grouping variables, defaults
#' to "Master.Protein.Accessions"
#' @keywords t-test p-value
#' @export
#' @examples
#' p_vals <- pval_ttest(stacked, "group1_log", "ctrl_log")

pval_ttest <- function (dat, group, ctrl, col = "Master.Protein.Accessions"){
  name <- NULL
  p_value <- NULL
  for (pro in unique(dat[, col])) {
    temp <- dplyr::filter(dat, dat[, col] == pro)
    pval_temp <- t.test(temp[, group], temp[, ctrl])$p.value
    name <- c(name, pro)
    p_value <- c(p_value, pval_temp)
  }
  return(as.data.frame(cbind(name, p_value)))
}
