#' abun_ratio
#'
#' Abundance ratio function
#' @param dat a data frame
#' @param group numerator
#' @param ctrl demoninator
#' @keywords group
#' @export
#' @examples
#' abun_ratio(df, "group1_med", "ctrl_med")


abun_ratio <- function (dat, group, ctrl = "ctrl_med"){
  dat[, group] / dat[, ctrl]
}
