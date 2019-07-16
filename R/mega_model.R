#' MEGA MODEL
#'
#' Creates a data frame with the change of individual isotopes over time
#' @param peptide peptide containing Leucine
#' @param deg_old degradation rate constant (k) of the "old" material around at the beginning of the experiment
#' @param deg_new degradation rate constant (k) of newly synthesized material
#' @param syn synthesis rate (amount/time)
#' @param t0_abun initial abundance
#' @param per_lab percent of D3-Leu in the labeled growth media
#' @param time vector of time to be modeled (\emph{e.g.} 0:168)
#' @keywords mega-model, peptide turnover, synthesis, degredation
#' @export
#' @examples
#' # initial conditions
#' peptide <- "SAMPLLLLLLER"
#'
#' # initial total abundance at T0
#' t0_abun <- 1000
#'
#' # fraction of labeled Leucine in the media
#' per_lab <- 0.50
#'
#' # rates
#' deg_old <- 0.0500
#' deg_new <- 0.0500
#' syn <- 50
#'
#' # length of time
#' time <- 0:168
#'
#' mod <- mega_model(peptide, deg_old, deg_new, syn, t0_abun, per_lab, time)

mega_model <- function (peptide, deg_old, deg_new, syn, t0_abun, per_lab, time){
  # create a data frame with a specified length of time
  mod <- data.frame("time" = time)

  # define the number of D3 Leucines that will be in the peptide and their
  # proportional syntheis rates based on the percent labeling
  D3_pep_dist <- iso_dist(peptide, p = per_lab)

  # define the pool names to be used as an argument in the make pools function
  pool_names <- c("D3_0_old_pool", "D3_0_new_pool",
                  paste("D3", 1:(length(D3_pep_dist) - 1), "pool", sep = "_"))

  # make pools for all
  pool_df <- make_pools(mod, pool_names, syn, deg_old, deg_new, t0_abun,
                        D3_pep_dist)

  # cbind to the data frame with the time vector
  mod <- cbind(mod, pool_df)

  # create a data frame with all isotopes for each pool
  df_all_isos <- all_isos(mod, pool_names, peptide)

  # cbind all columns together
  mod <- cbind(mod, df_all_isos)

  # sum all the individual isotopes to get the totals
  mod <- cbind(mod, combine_iso(mod))

  return(mod)
}
