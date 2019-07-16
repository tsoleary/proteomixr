#' Proteome
#'
#' Creates a data frame with an average abundance of the top ionizers for each protein
#' @param file_name file containing all peptides for all samples with the columns named Annotated.Sequence, Modifications, Master.Protein.Accessions.
#' @param organism "mouse" or "human"
#' @param wt_samps "all" if you want to use all samples to weight, otherwise use a pattern that is present in the column names of the samples you wish to use for the weighting
#' @param group if you want to
#' @param group_names indicate names of the groups (must be a string pattern present in the samples)
#' @param top_pep number of top ionizers used
#' @param min_pep minumum number of peptides found in a protein to be incuded in the analysis
#' @param norm set to TRUE if normalization is wanted
#' @param norm_method indicate normalization method: "protein" or "sum_total"
#' @param norm_pro accession of protein used for
#' @param csv if result file should be written in working directory
#' @keywords whole proteome analysis
#' @export
#' @examples
#' proteome("WT vs KO all pep.csv", group_names = c("WT", "KO"),
#'          organism = "mouse", group = TRUE, norm = TRUE,
#'          norm_method = "protein", norm_pro = "B2RTM0", csv = FALSE)

proteome <- function(file_name, organism,
                     wt_samps = "all", group = FALSE, group_names = NULL,
                     top_pep = 3, min_pep = 3,
                     norm = FALSE, norm_method = NULL, norm_pro = NULL,
                     csv = TRUE){

  # import data frame
  dat <- read.csv(file_name)

  # data frame format check
  if (colnames(dat)[1] != "Annotated.Sequence" &
      colnames(dat)[2] != "Modifications" &
      colnames(dat)[3] != "Master.Protein.Accessions")
    stop ("invalid data frame: must have Annotated.Sequence, Modifications,
         Master.Protein.Accessions")

  # # import gene accession ####need to fix paths for PREVIS computer!!!
  # if (organism == "mouse") {
  # gene_df <- read.csv(
  #   "C:/Users/PrevBeast/Documents/Fasta Files/mouse_fasta_gene_accession.csv")
  # }
  # if (organism == "human"){
  #   gene_df <- read.csv(
  #     "C:/Users/PrevBeast/Documents/Fasta Files/human_fasta_gene_accession.csv")
  # }
  # if (organism != "mouse" & organism != "human")
  #   stop ("only human or mouse organism currently supported")

  # import gene accession
  if (organism == "mouse") {
    gene_df <- read.csv(
      "/Users/tsoleary/previs/mouse_PD_accession_gene.csv")
  }
  if (organism == "human"){
    gene_df <- read.csv(
      "C:/Users/PrevBeast/Documents/Fasta Files/human_fasta_gene_accession.csv")
  }
  if (organism != "mouse" & organism != "human")
    stop ("only human or mouse organism currently supported")


  # determine the top ionizers and sort by them taking only the top_pep
  if (wt_samps == "all"){
    wt_grp <- 4:ncol(dat)
  } else {
    if (sum(grepl(wt_samps, colnames(dat))) < 1)
      stop ("wt_samps must be a pattern that is present in the sample
           (column) names that are being used to weight the top ionizers")
    wt_grp <- grep(wt_samps, colnames(dat))
  }

  dat$wt_grp <- by_group(dat, wt_grp)

  df <- tbl_df(dat) %>%
    group_by(Master.Protein.Accessions) %>%
    top_n(n = top_pep, wt = wt_grp)


  # do normalization if indicated --------
  if (norm == TRUE){

    if (norm_method == "sum_total") {
      norm_value <- colSums(df[, wt_grp], na.rm = TRUE)

      raw_abun_mat <- as.matrix(df[, wt_grp])

      norm_abun <- t(t(raw_abun_mat)/norm_value)
      colnames(norm_abun) <- paste(colnames(norm_abun), sep = "_", "norm")
      norm_test <- as.data.frame(norm_abun)
      df <- tibble::as_tibble(df)
      df <- cbind(df, norm_test)

    }

    if (norm_method == "protein"){

      norm_pep <- subset(df, df$Master.Protein.Accessions == norm_pro)
      numeric_cols <- which(sapply(norm_pep, is.numeric) == TRUE)
      raw_abun <- numeric_cols[-length(numeric_cols)]
      norm_value <- sapply(norm_pep[, raw_abun], mean, na.rm = TRUE)
      raw_abun_mat <- as.matrix(df[, raw_abun])
      norm_abun <- t(t(raw_abun_mat)/norm_value)
      colnames(norm_abun) <- paste(colnames(norm_abun), sep = "_", "norm")
      norm_test <- as.data.frame(norm_abun)
      df <- tibble::as_tibble(df)
      df <- cbind(df, norm_test)


    }
  }

  df <- as.data.frame(df)

  # grouping
  if (group == FALSE & norm == FALSE){
    group_names <- "F"
  }

  if (group == TRUE){
    for (i in 1:length(group_names)){

      if (norm == TRUE){
        pat <- paste0(group_names[i], "_norm")
      } else {
        pat <- group_names[i]
      }

      samp_col_group <- grep(pat, colnames(df))

      df[, paste0(group_names[i], "_med")] <- by_group(df, samp_col_group)

    }
    pro_df <- by_protein(df, colnames(df)[grep("_med", colnames(df))]) %>%
      as.data.frame %>%
      rownames_to_column("Master.Protein.Accessions")

    colnames(pro_df) <- sub("_med", "", colnames(pro_df))
  } else {

    all <- colnames(dat)[-c(1,2,3,length(dat))]

    pro_df <- by_protein(df, all) %>%
      as.data.frame %>%
      rownames_to_column("Master.Protein.Accessions")

  }

  pro_df$gene <- mpa_to_gene(pro_df, gene_df)

  pro_df$peptides <- table(dat$Master.Protein.Accessions)
  pro_df <- dplyr::filter(pro_df, pro_df$peptides >= min_pep)

  col_order <- c("gene", "Master.Protein.Accessions", "peptides",
                 colnames(pro_df)[2:(length(colnames(pro_df))-2)])

  pro_df <- pro_df[, col_order]

  colnames(pro_df)[1] <- "Gene"
  colnames(pro_df)[2] <- "Accession"
  colnames(pro_df)[3] <- "# of peptides"

  pro_df <- filter(pro_df, Accession != "")

  file_output <- paste0(gsub(".csv", "", file_name), "_r_",
                        format(Sys.time(), "%d %b %Y %H:%M:%S"),
                        ".csv")

  if (csv == TRUE){
    write.csv(pro_df, file_output, row.names = FALSE)
    print(paste("result file", file_output ,"is located in", getwd()))
  }
  pro_df <- tibble::as_tibble(pro_df)
  return(pro_df)
}
