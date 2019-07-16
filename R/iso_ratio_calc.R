#' "Corrected" Isotope Ratio
#'
#' Creates a vector containing the abundance ratio for each peptide
#' @param dat a data frame containing the grouping variables, the isotopes and the T0 ratio
#' @keywords corrected isotope ratio
#' @export
#' @examples
#' df$ratio <- iso_ratio_calc(df)

iso_ratio_calc <- function(dat){

  ratios <- NULL

  for (i in 1:nrow(dat)){

    t0_ratio <- mean(dat$t0_r[which(dat$protein == dat$protein[i] &
                                      dat$peptide == dat$peptide[i] &
                                      dat$sex == dat$sex[i] &
                                      dat$leg == dat$leg[i] &
                                      dat$week == 0)], na.rm = TRUE)

    temp <- (dat$M_1[i] + dat$M_2[i] + dat$M_3[i] - (t0_ratio * dat$M_0[i])) /
      (dat$M_0[i] + (dat$M_1[i] + dat$M_2[i] + dat$M_3[i] -
                       (t0_ratio * dat$M_0[i])))

    ratios <- c(ratios, temp)

  }

  return(ratios)

}
