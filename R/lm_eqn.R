#' Linear Regression Equation Object
#'
#' Creates an character vector with the y = mx + b equation, r-squared, and p-value, to be used for adding to plots
#' @param lm_object
#' @keywords equation, linear regression, r-squared, p-value, plot
#' @export
#' @examples
#' df_l <- filter(dat, leg == "L")
#' lm_l <- lm(abundance ~ week, df_l)
#' eqn_l <- lm_eqn(lm_l)





# get the regression eqn, r_sq, & pval for plotting from a lm_object
lm_eqn <- function(lm_object) {
  eq <-
    substitute(
      italic(y) == m ~ italic(x) + b,
      list(
        b = as.character(signif(coef(lm_object)[1], digits = 2)),
        m = as.character(signif(coef(lm_object)[2], digits = 2))
      )
    )
  r <-
    substitute(
      italic(r) ^ 2 ~ "=" ~ r2,
      list(
        r2 = as.character(signif(summary(lm_object)$r.squared, digits = 3))
      )
    )
  p <-
    substitute(
      italic("p-val") ~ "=" ~ pval,
      list(
        pval = as.character(signif(anova(lm_object)$'Pr(>F)'[1], digits = 3))
      )
    )
  result <- c(as.character(as.expression(eq)), as.character(as.expression(r)),
              as.character(as.expression(p)))
  return(result)
}
