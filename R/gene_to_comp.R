#' Groups proteins by gene symbols into compartments or sub-compartments
#'
#' Takes data frame with a "gene" column and adds a column using a second data
#' frame as a reference to convert between "Gene" column and a compartment
#' column
#' @param dat a data frame with gene symbols
#' @param gene_dat a reference data frame with gene symbols and the assigned
#' compartments and sub-compartments within the cell
#' @param level a string containing the level to be sorted
#' @keywords sub-cellular compartments
#' @export
#' @examples
#' df$sub_compartment <- gene_to_comp(df, comp_df, level = "sub_compartment")

gene_to_comp <- function (dat, comp_dat, level = "compartment"){
  dat$comp <- dat$gene
  for (i in 1:nrow(dat)){
    temp <- which(dat$gene[i] == comp_dat$Gene, TRUE)
    if (length(temp) == 1){
      dat$comp <- gsub(dat$comp[i],
                       comp_dat[temp, level],
                       dat$comp, ignore.case = TRUE)
    }
  }
  return(dat$comp)
}

