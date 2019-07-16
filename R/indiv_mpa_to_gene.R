#' Converts Accession to Gene Name for Individual Proteins
#'
#' Looks up a accession in a gene_df and finds the corresponding gene name
#' @param Acc_pro accession number to be converted to gene
#' @param gene_dat reference data frame with Master.Protein.Accessions and gene columns
#' @keywords master protein accession, gene
#' @export
#' @examples
#' gene <- indiv_mpa_to_gene(pro, protein)


indiv_mpa_to_gene <- function (Acc_pro, gene_dat){
  temp <- which(Acc_pro == gene_dat$Master.Protein.Accessions, TRUE)
  gene <- gsub(Acc_pro, gene_dat$gene[temp], Acc_pro)
  return(gene)
}
