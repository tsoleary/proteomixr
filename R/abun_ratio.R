#' Abundance ratio
#'
#' Creates a vector containing the abundance ratio for each peptide
#' @param dat a data frame containing columns with median peptide abundances
#' @param group column string or number (numerator of ratio)
#' @param ctrl column string or number (demoninator of ratio)
#' defaults to "ctrl_med"
#' @keywords ratio
#' @export
#' @examples
#' df$ratio <- abun_ratio(df, "group1_med")


abun_ratio <- function (dat, group, ctrl = "ctrl_med"){
  dat[, group] / dat[, ctrl]
}
