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
#' @keywords t-test p-value
#' @export
#' @examples
#' p_vals <- pval_ttest(stacked, "group1_log", "ctrl_log")

pval_ttest <- function (dat, group, ctrl){
  Master.Protein.Accessions <- NULL
  p_value <- NULL
  for (pro in unique(dat$Master.Protein.Accessions)){
    temp <- dplyr::filter(dat, dat$Master.Protein.Accessions == pro)
    pval_temp <- t.test(temp[, group], temp[, ctrl])$p.value
    Master.Protein.Accessions <- c(Master.Protein.Accessions, pro)
    p_value <- c(p_value, pval_temp)
  }
  return(as.data.frame(cbind(Master.Protein.Accessions, p_value)))
}
