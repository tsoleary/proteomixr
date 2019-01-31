#' mpa_to_gene
#'
#' Convert Master.Protein.Accession to gene symbol
#' @param dat a data frame with accession numbers
#' @param gene_dat a reference data frame with all accession numbers and gene symbols
#' @keywords gene symbol
#' @export
#' @examples
#' mpa_to_gene(df, gene_df)


mpa_to_gene <- function (dat, gene_dat){
  dat$gene <- dat$Master.Protein.Accessions
  for (i in 1:nrow(dat)){
    temp <- which(dat$gene[i] == gene_dat$Accession, TRUE)
    if (length(temp) == 1){
      dat$gene <- gsub(dat$gene[i], gene_dat$Gen[temp], dat$gene)
    }
  }
  return(dat$gene)
}
