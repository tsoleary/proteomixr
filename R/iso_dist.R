#' Distribution of D3-Leu peptides
#'
#' Calculates the distribution of newly synthesized D3-Leu peptides
#' @param pep peptide, must contain at least one "L"
#' @param p the percent of D3-labeled Leucine in the labeled media, defaults to 0.5
#' @keywords D3 Leucine, labeling, peptide, synthesis
#' @section Warning:
#' p cannot equal 1, if the percent label is 100%, then write p = 0.9999
#' @export
#' @examples
#' iso_dist("SAMPLLER", p = 0.99)

iso_dist <- function (pep, p = 0.5){

  L <- stringr::str_count(pep, pattern = "L")
  isos <- 0:L
  dis <- NULL

  for (i in isos){
    temp <- (factorial(L)/(factorial(L-i)*factorial(i)))*(p^i)*(1-p)^(L-i)
    dis <- c(dis, temp)
  }
  return(dis)
}



