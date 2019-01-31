#' Peptide average or standard deviation
#'
#' Peptide level average or sd of a group of samples
#' @param dat a data frame with peptide level data as rows and samples in
#' columns
#' @param col a numeric vector containing the column numbers of samples within
#' the group
#' @keywords group median sd
#' @export
#' @examples
#' # selects the columns containing a string that identifies the group
#' group1 <- grep("Sample_norm", colnames(data))
#' group1_sd <- by_group(df, group1, FUN = sd)

by_group <- function (dat, col, FUN = median){
  list <- NULL
  for (i in 1:nrow(dat)){
    temp <- FUN(as.numeric(dat[i, col], na.rm = TRUE))
    list <- c(list, temp)
  }
  return(list)
}
