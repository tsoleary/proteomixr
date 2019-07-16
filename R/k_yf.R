#' K constant and Plateau from Non-Linear Fit
#'
#' Creates an object with the k and yf values
#' @param nls_df non-linear fit data frame
#' @param leg which leg group
#' @keywords non-linear fit, plot, k, plateau
#' @export
#' @examples
#'   df <- dat %>%
#'group_by(leg) %>%
#'   do(fit = nls(ratio ~ SSasymp(week, yf, y0, log_k), data = .)) %>%
#'   tidy(fit) %>%
#'   select(leg, term, estimate) %>%
#'   spread(term, estimate) %>%
#'   mutate(k = exp(log_k))
#'
#' L_k_yf <- k_yf(df, "L")

k_yf <- function(nls_df, leg) {
  k <-
    substitute(
      italic(k) ~ "=" ~ k_val,
      list(
        k_val = as.character(signif(nls_df$k[which(nls_df$leg == leg)],
                                    digits = 3))
      )
    )
  yf <-
    substitute(
      italic(yf) ~ "=" ~ yf_val,
      list(
        yf_val = as.character(signif(nls_df$yf[which(nls_df$leg == leg)],
                                     digits = 2))
      )
    )
  result <- c(as.character(as.expression(k)), as.character(as.expression(yf)))
  return(result)
}
