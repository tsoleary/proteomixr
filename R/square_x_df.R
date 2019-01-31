#' Pooled standard deviation calculation intermediate
#'
#' Multiplies the square of the sd by the degrees of freedom. Used as an
#' intermediate as part of the calculation to get the pooled standard deviation
#' of a protein group. Returns a vector with the pooled sd calc intermediate
#' @param dat a data frame with peptide level data as rows and samples in columns
#' @param sd peptide level sd of a ratio or median abundance
#' @param df degrees of freedom
#' @keywords sd calc pooled standard deviation
#' @export
#' @examples
#' df$sd_calc <- square_x_df(df,"group1_sd", "group1_df")

square_x_df <- function (dat, sd, df){
  (dat[, sd])^2 * (dat[, df])
}
