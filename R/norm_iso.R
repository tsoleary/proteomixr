#' Normalize Isotopes to the Sum
#'
#' Adds columns of normalized isotopes
#' @param dat a data frame containing columns of all individual isotopes ("M_0" \emph{etc.}) and "Sum"
#' @keywords normalize isotopes
#' @export
#' @examples
#' df <- df_tidy %>%
#'   spread(., "isotope", "abundance") %>%
#'   as.data.frame(.) %>%
#'   norm_iso(.)

norm_iso <- function(dat){
  isotopes <- colnames(dat)[grep("M_", colnames(dat))]
  df <- NULL
  for(iso in isotopes){
    temp <- dat[iso]/dat["Sum"]
    df <- c(df, temp)
  }
  df <- as.data.frame(df)
  colnames(df) <- paste0(isotopes, "_norm")
  dat <- cbind(dat, df)
}
