#' Remove outliers in peptide data
#'
#' Creates a new data frame with the outliers from original data frame removed.
#' Outliers are defined as ratios
#' @param dat a data frame
#' @param pro_df a data fram containing average ratio of each protein
#' @param ratio a character string of the column name of the ratio used to find
#' outliers
#' @param mult multiplier of the sd used to determine the min and max allowable
#' ratio for each peptide, defaults to 2
#' @keywords remove outliers
#' @import
#' @export
#' @examples
#' data_rm_out <- rm_outliers(df, pro_df, "ratio")

rm_outliers <- function (dat, pro_df, ratio, mult = 2){

  sd_ratio_temp_df <- by_protein(dat, ratio, FUN = sd) %>%
    as.data.frame %>%
    rownames_to_column("Master.Protein.Accessions") %>%
    'colnames<-' (c("Master.Protein.Accessions", "sd_ratios"))

  pro_temp <- dplyr::full_join(pro_df, sd_ratio_temp_df,
                               by = "Master.Protein.Accessions")

  pro_temp$max_ratio <- pro_temp$ratio + mult * pro_temp$sd_ratio
  pro_temp$min_ratio <- pro_temp$ratio - mult * pro_temp$sd_ratio

  data_rm_out <- NULL

  for (pro in unique(dat$Master.Protein.Accessions)){
    temp <- dplyr::filter(dat, dat$Master.Protein.Accessions == pro)

    rm_high <- which(temp$ratio > pro_temp$max_ratio[which(
      pro_temp$Master.Protein.Accessions == pro)])

    rm_low <- which(temp$ratio < pro_temp$min_ratio[which(
      pro_temp$Master.Protein.Accessions == pro)])

    rm <- c(rm_high, rm_low)
    if (length(rm) > 0){
      temp_rm <- temp[-rm, ]
      data_rm_out <- dplyr::bind_rows(data_rm_out, temp_rm)
    } else {
      data_rm_out <- dplyr::bind_rows(data_rm_out, temp)
    }
  }
  return(data_rm_out)
}
