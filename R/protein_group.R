#' by_protein
#'
#' Groups by protein
#' @param dat a data frame with peptide level data as rows and samples in columns
#' @param groups a numeric vector containing the column numbers of the group
#' that the function will be performed on
#' @keywords group by protein
#' @export
#' @examples
#' protein_group(df, c("ctrl_med", "group1_med"))

by_protein <- function (dat, groups, FUN = mean){
  tab <- NULL
  for (col in groups){
    temp <- tapply(dat[, col],
                   dat$Master.Protein.Accessions,
                   FUN,
                   na.rm = TRUE)
    tab <- cbind(tab, temp)
  }
  colnames(tab) <- groups
  return(tab)
}
