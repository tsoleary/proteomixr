#' Plot Isotopic Ratio
#'
#' Plots the changing corrected isotopic ratio over time
#' @param dat a data frame with ratio and week columns
#' @param g_title title of the graph
#' @param g_subtitle subtitle of the graph
#' @param FUN plotting function, geom_jitter or geom_point
#' @param x_pos,y_pos1,y_pos2 x and y positions of the k and plateau values
#' @keywords plot isotopic ratio, corrected
#' @export
#' @examples
#' plot_iso_r(df_g, g_title = "Protein", g_subtitle = "PEPTIDE")

plot_iso_r <- function(dat, g_title, g_subtitle, FUN = geom_jitter,
                       x_pos = 3.35, y_pos1 = 0.83, y_pos2 = 0.30){

  df <- dat %>%
    group_by(leg) %>%
    do(fit = nls(ratio ~ SSasymp(week, yf, y0, log_k), data = .)) %>%
    tidy(fit) %>%
    select(leg, term, estimate) %>%
    spread(term, estimate) %>%
    mutate(k = exp(log_k))

  L_k_yf <- k_yf(df, "L")
  R_k_yf <- k_yf(df, "R")

  g <- ggplot(dat, aes(x = week, y = ratio)) +
    FUN(mapping = aes(x = week, y = ratio, fill = leg),
        alpha = 0.5, size = 3, pch = 21,  color = "black", width = 0.05) +
    geom_smooth(method = "nls", formula = y ~ SSasymp(x, yf, y0, log_k),
                data = dat,
                se = FALSE,
                aes(color = leg), show.legend = FALSE) +
    annotate("text", x = x_pos, y = (y_pos1)*(max(dat$ratio)),
             label = L_k_yf[1], parse = TRUE, color = "#F98B86") +
    annotate("text", x = x_pos, y = (y_pos1 - 0.07)*(max(dat$ratio)),
             label = L_k_yf[2], parse = TRUE, color = "#F98B86") +
    annotate("text", x = x_pos, y = (y_pos2)*(max(dat$ratio)),
             label = R_k_yf[1], parse = TRUE, color = "#53D3D7") +
    annotate("text", x = x_pos, y = (y_pos2 - 0.07)*(max(dat$ratio)),
             label = R_k_yf[2], parse = TRUE, color = "#53D3D7") +
    labs(title = g_title, subtitle = g_subtitle,
         x = "Week", y = "Isotopic Ratio\nCorrected", # change later!
         fill = "Leg") +
    expand_limits(x = 0, y = 0) +
    theme_classic() +
    coord_cartesian(xlim = c(0, 3), clip = 'off') +
    theme(plot.margin = unit(c(1, 2.25, 1, 1), "lines"))
  return(g)

}
