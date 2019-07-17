#' Isotopic Distribution for all Pools
#'
#' Creates a vector containing the abundance ratio for each peptide
#' @param dat the model data frame
#' @param pool_cols all the pools in the model
#' @param peptide character string of the peptide
#' @keywords isos, mega model
#' @export
#' @examples
#' all_isos(mod, pools, peptide)

all_isos <- function(dat, pool_cols, peptide){

  temp <- NULL
  df <- NULL

  for (pools in pool_cols){
    temp <- pool_iso_dist(dat, pools, peptide)
    df <- cbind(df, temp)
  }
  return(df)
}
