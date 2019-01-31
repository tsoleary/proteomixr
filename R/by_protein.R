#' FUN by protein
#'
#' Takes a data frame and performs a function on each protein group within
#' "Master.Protein.Accessions" for specified groups of samples
#' @param dat a data frame with peptide level data as rows and samples in
#' columns
#' @param groups a vector of character strings or
#' @param FUN function argument for tapply() defaults to mean
#' @keywords group by protein
#' @export
#' @examples
#' med_groups <- c("ctrl_med", "group1_med")
#' protein <- by_protein(df, med_groups)

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
