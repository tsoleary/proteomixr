#' Isotopic Distribution of a Pool
#'
#' Multiplies a pool by the isotopic distribution
#' @param dat data frame with the pool in it
#' @param pool specific D3_0 \emph{etc.} pool
#' @param peptide peptide
#' @keywords pool, isotopic distribution
#' @export
#' @examples
#' temp <- pool_iso_dist(dat, pools, peptide) # as used in all_isos() function

pool_iso_dist <- function (dat, pool, peptide) {

  isos <- paste0("M_", seq(((as.numeric(gsub("^D3_([0-9]+)_.*",
                                             "\\1", pool)))*3),
                           by = 1, length = 9))

  nat_iso <- pep_iso(peptide)

  temp <- NULL
  df <- NULL
  for (i in 1:length(isos)){
    temp <- dat[, pool] * nat_iso[i, "per_total"]
    df <- cbind(df, temp)
  }

  colnames(df) <- paste(str_replace(pool, "_pool", ""), isos, sep = "_")

  return(df)
}
