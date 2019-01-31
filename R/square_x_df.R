#' square_x_df
#'
#' Standard deviation of grouped relative abundance using top ionizers
#' @param dat a data frame with peptide level data as rows and samples in columns
#' @param sd
#' @param df
#' @keywords sd calc
#' @export
#' @examples
#' square_x_df(df,"group1_sd", "group1_df")

square_x_df <- function (dat, sd, df){
  (dat[, sd])^2 * (dat[, df])
}
