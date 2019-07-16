#' Linear fit result
#'
#' Creates a data frame with intercept, slope, r^2, and p-value columns
#' @param dat a data frame containing abundance versus week
#' @keywords linear fit, intercept, slope, r-squared
#' @export
#' @examples
#' df_fit <- df %>%
#'   group_by(Master.Protein.Accessions, sex, leg) %>%
#'   do(lin_fit(.))

lin_fit <- function(dat){
  the_fit <- lm(dat$abundance ~ dat$week, dat)
  p_val <- anova(the_fit)$'Pr(>F)'[1]
  slo_int <- data.frame(t(coef(the_fit)))
  r_sq <- summary(the_fit)$r.squared
  result <- cbind(slo_int, r_sq, p_val)
  colnames(result) <- c("intercept", "slope", "r_squared", "p_value")
  return(result)
}
