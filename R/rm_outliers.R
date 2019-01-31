#' rm_outliers
#'
#' Remove outliers in peptide data
#' @param dat a data frame
#' @param pro_df a data fram containing average ratio by protein
#' @param ratio specific ratio to remove outliers by
#' @keywords group
#' @export
#' @examples
#' rm_outliers(df, pro_df, "ratio")

rm_outliers <- function (dat, pro_df, ratio){

  sd_ratio_temp_df <- protein_group(dat, ratio, FUN = sd) %>%
    as.data.frame %>%
    rownames_to_column("Master.Protein.Accessions") %>%
    'colnames<-' (c("Master.Protein.Accessions", "sd_ratios"))

  pro_temp <- full_join(pro_df, sd_ratio_temp_df,
                        by = "Master.Protein.Accessions")

  pro_temp$max_ratio <- pro_temp$ratio + 2 * pro_temp$sd_ratio
  pro_temp$min_ratio <- pro_temp$ratio - 2 * pro_temp$sd_ratio

  data_rm_out <- NULL

  for (pro in unique(dat$Master.Protein.Accessions)){
    temp <- filter(dat, dat$Master.Protein.Accessions == pro)

    rm_high <- which(temp$ratio > pro_temp$max_ratio[which(
      pro_temp$Master.Protein.Accessions == pro)])

    rm_low <- which(temp$ratio < pro_temp$min_ratio[which(
      pro_temp$Master.Protein.Accessions == pro)])

    rm <- c(rm_high, rm_low)
    if (length(rm) > 0){
      temp_rm <- temp[-rm, ]
      data_rm_out <- bind_rows(data_rm_out, temp_rm)
    } else {
      data_rm_out <- bind_rows(data_rm_out, temp)
    }
  }
  return(data_rm_out)
}