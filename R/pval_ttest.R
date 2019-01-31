#' t-test function between groups
#'
#' This function groups together
#' @param dat a data frame stacked
#' @param group
#' @param ctrl
#' @keywords ttest
#' @export
#' @examples
#' pval_ttest(df, "group1", "group2")

pval_ttest <- function (dat, group, ctrl){
  Master.Protein.Accessions <- NULL
  p_value <- NULL
  for (pro in unique(dat$Master.Protein.Accessions)){
    temp <- filter(dat, dat$Master.Protein.Accessions == pro)
    pval_temp <- t.test(temp[, group], temp[, ctrl])$p.value
    Master.Protein.Accessions <- c(Master.Protein.Accessions, pro)
    p_value <- c(p_value, pval_temp)
  }
  return(as.data.frame(cbind(Master.Protein.Accessions, p_value)))
}
