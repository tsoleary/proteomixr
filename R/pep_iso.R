#' Peptide Isotopic Distribution
#'
#' Returns a data frame with the relative abundances of each isotope in a peptide
#' @param pep peptide
#' @param max_iso number of isotopes to calculate
#' @param charge charge of the peptide in MS data
#' @section Warning:
#' Does not yet have capability to handle modified peptides (e.g. oxidation)
#' @keywords peptide isotopic distribution
#' @export
#' @examples
#' pep_iso("SAMPLER", max_iso = 6, charge = 2)



# isotope distribution for a peptide - data frame output
pep_iso <- function (pep, max_iso = 9, charge = 1){
  pep_length <- nchar(pep)
  pep_bond <- pep_length - 1

  aa_sep <- NULL
  for (i in 1:pep_length){
    aa <- substr(pep, i, i)
    aa_sep <- c(aa_sep, aa)
  }

  # reference data frame with molecular formulas of all amino acids
  aa_form <- data.frame("letter" = c("A", "R", "N", "D", "C", "Q", "E", "G",
                                     "H", "I", "L", "K", "M", "F", "P", "S",
                                     "T", "W", "Y", "V"),
                        "molecular_formula" = c("C3H7NO2", "C6H14N4O2",
                                                "C4H8N2O3", "C4H7NO4",
                                                "C3H7NO2S", "C5H10N2O3",
                                                "C5H9NO4", "C2H5NO2",
                                                "C6H9N3O2", "C6H13NO2",
                                                "C6H13NO2", "C6H14N2O2",
                                                "C5H11NO2S", "C9H11NO2",
                                                "C5H9NO2", "C3H7NO3",
                                                "C4H9NO3", "C11H12N2O2",
                                                "C9H11NO3", "C5H11NO2"))

  mols <- NULL
  for (j in 1:length(aa_sep)){
    row <- grep(aa_sep[j], aa_form$letter)
    mol <- as.character(aa_form[row, "molecular_formula"])
    mols <- paste0(mols, mol)
  }

  pep_tot <- Rdisop::getMolecule(mols)$formula

  element <- function(formula){
    # pattern to match the initial element assumes element starts with an upper
    # case and optional lower case followed by zero or more digits.
    first <- "^([[:upper:]][[:lower:]]?)([0-9]*).*"
    # inverse of above to remove the initial element
    last <- "^[[:upper:]][[:lower:]]?[0-9]*(.*)"
    result <- list()
    extract <- formula
    # repeat as long as there is data
    while ((start <- nchar(extract)) > 0){
      chem <- sub(first, '\\1 \\2', extract)
      extract <- sub(last, '\\1', extract)
      # if the number of characters is the same, then there was an error
      if (nchar(extract) == start){
        warning("Invalid formula:", formula)
        return(NULL)
      }
      # append to the list
      result[[length(result) + 1L]] <- strsplit(chem, ' ')[[1]]
    }
    result
  }

  ans <- unlist(element(pep_tot))

  if (ans[length(ans)] == "S"){
    ans <- c(ans, "0")
  }

  elem <- ans[rep(seq(from = 1, to = length(ans), by = 2), 1)]
  num <- ans[rep(seq(from = 2, to = length(ans), by = 2), 1)]

  df <- as.data.frame(cbind(elem, num))
  df$num <- as.numeric(as.character(df$num))

  # correcting for loss of water in the peptide bond
  df[elem == "H", "num"] <- (df[elem == "H", "num"] - (2 * pep_bond) + charge)
  df[elem == "O", "num"] <- (df[elem == "O", "num"] - 1 * pep_bond)

  # convert from data frame back to molecular fomula string
  df_mat <- as.matrix(df)
  df_list <- NULL
  for (k in seq(1:nrow(df_mat))){
    df_list <- c(df_list, df_mat[k, ])
  }
  pep_mol <- gsub(" ", "",
                  paste(as.character(df_list), sep = "", collapse = ""))

  # get isotope on final molecule
  pep_molecule <- Rdisop::getMolecule(pep_mol, z = charge)
  pep_dist <- as.data.frame(getIsotope(pep_molecule, seq(1, max_iso)),
                            row.names = c("m_z", "per_total"))
  colnames(pep_dist) <- paste0("M_", 0:(max_iso-1))
  pep_dist <- as.data.frame(t(pep_dist))
  pep_dist$m_z <- (pep_dist$m_z / charge)
  pep_dist <- round(pep_dist, 3)

  return(pep_dist)

}
