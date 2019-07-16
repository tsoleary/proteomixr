#' Make pools for D3_0_old, D3_0_new, D3_1, D3_2 \emph{etc.} Pools
#'
#' Part of \strong{MEGA MODEL}
#' @param dat data frame
#' @param pool_names vector containing the pool names for each pool of labeled Leucines
#' @param syn synthesis rate of new material
#' @param deg_old degredation constant for the "old" material present at T0
#' @param deg_new degredation constant for the newly synthesized material
#' @param t0_abun abundance at the start (T0)
#' @param D3_pep numeric vector containing the result from iso_dist
#' @keywords synthesis, degredation, pools
#' @export
#' @examples
#' pool_df <- make_pools(mod, pool_names, syn, deg_old, deg_new, t0_abun,
#'                       D3_pep_dist)

make_pools <- function(dat, pool_names, syn, deg_old, deg_new, t0_abun, D3_pep){

  temp <- NULL
  df <- NULL

  for (pool in pool_names){
    if (grepl("old", pool) == TRUE){
      f_syn <- 0
      f_deg <- deg_old
      f_initial <- t0_abun
    } else {
      f_syn <- syn
      f_deg <- deg_new
      f_initial <- 0
    }

    temp <- deg_syn(dat, f_deg, f_syn, f_initial, D3_pep,
                    D3_i = as.numeric(gsub("^D3_([0-9]+)_.*", "\\1", pool)))
    df <- cbind(df, temp)
  }
  colnames(df) <- pool_names
  return(df)
}
