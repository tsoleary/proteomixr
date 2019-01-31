#' Convert master protein accession to gene symbol
#'
#' Takes data frame with a "Master.Protein.Accession" column and adds a column
#' "gene" using a second data frame as a reference to convert between an
#' "Accession" column and a "Gene" symbol column
#' @param dat a data frame with only master protein accession numbers
#' @param gene_dat a reference data frame with all accession numbers and
#' gene symbols found within the data set
#' @keywords gene symbol
#' @export
#' @examples
#' df$gene <- mpa_to_gene(df, gene_df)


mpa_to_gene <- function (dat, gene_dat){
  dat$gene <- dat$Master.Protein.Accessions
  for (i in 1:nrow(dat)){
    temp <- which(dat$gene[i] == gene_dat$Accession, TRUE)
    if (length(temp) == 1){
      dat$gene <- gsub(dat$gene[i], gene_dat$Gene[temp], dat$gene)
    }
  }
  return(dat$gene)
}
