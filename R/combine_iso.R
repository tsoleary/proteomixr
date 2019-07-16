#' Sums Isotopes from Different Pools
#'
#' Combines by summation each isotope from different pools
#' @param dat a data frame with multiple pools of individual isotopes to be summed
#' @keywords isotope, sum total
#' @export
#' @examples
#' mod <- cbind(mod, combine_iso(mod)) # as used in mega_model()


# coombine the individual isotopomers from different pools back to a total
combine_iso <- function (dat){

  # removes the M_ from each column name?
  colnames(dat) <- gsub("^.*(M_[0-9]+)", "\\1", colnames(dat))

  # combine all columns that have the same name by their
  df <- sapply(split.default(dat, colnames(dat)),
               rowSums, na.rm = TRUE)

  # add the word total to every colname with a M_0 pattern
  colnames(df) <- gsub("^.*(M_[0-9]+)",
                       paste0("\\1", "_total"), colnames(df))

  # remove pool and time from the df
  df <- df[, grep(c("M_"), colnames(df))]

  # pading with a 0 to allow sorting of the columns
  colnames(df) <- stringr::str_pad(gsub("M_", "", colnames(df)), 8, pad = "0")

  df <- df[, sort(colnames(df))]

  colnames(df) <- c(paste0("M_", colnames(df)))

  return(df)

}
