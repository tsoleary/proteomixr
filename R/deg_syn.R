#' Degradation and Synthesis of a Peptide Over Time
#'
#' How the pools change over time
#' @param dat a data frame containing columns with median peptide abundances
#' @param deg degradation constant (k)
#' @param syn synthesis rate (amount/time)
#' @param initial abundance at T0
#' @param D3_pep numeric vector containing the result from iso_dist
#' @param D3_i specific isotope (\emph{i.e.} 0 for "M+0", 1 for "M+1" \emph{etc.})
#' @keywords degradation, synthesis
#' @export
#' @examples
#' deg_syn(dat, f_deg, f_syn, f_initial, D3_pep,
#'         D3_i = as.numeric(gsub("^D3_([0-9]+)_.*", "\\1", pool)))

deg_syn <- function (dat, deg, syn, initial, D3_pep, D3_i){

  temp <- NULL
  list <- NULL
  D3_syn <- syn * D3_pep[D3_i + 1]

  for (i in 1:((nrow(dat) - 1))){
    if (is.null(temp) == TRUE){
      temp <- initial
      list<- c(temp)
    }
    temp <- temp - (temp * deg) + D3_syn
    list <- c(list, temp)
  }
  return(list)
}
