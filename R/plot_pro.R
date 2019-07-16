#' Plot proteins changing overtime
#'
#' Plots changing protein abundance over time for two different groups
#' @param dat a data frame containing abundance and week columns
#' @param g_title title of the plot
#' @param FUN geom_jitter or geom_point
#' @param x_pos,y1_pos,y2_pos x and y coordinates of the linear eqn and r-squared objects
#' @keywords
#' @export
#' @examples
#' pros <- as.character(unique(df$Master.Protein.Accessions))
#'
#' plot_list <- list()
#'
#' for (pro in pros){
#'   gene <- indiv_mpa_to_gene(pro, protein)
#'   temp_df <- filter(df, Master.Protein.Accessions == pro)
#'   g <- plot_pro(temp_df, g_title = gene)
#'   plot_list[[pro]] <- g
#' }
#'
#' pdf("all_plots.pdf", width = 10.75, height = 6)
#'
#' for(pro in pros){
#'   print(plot_list[[pro]])
#' }

plot_pro <- function(dat, g_title, FUN = geom_jitter, x_pos = 9, y1_pos = 0.83,
                     y2_pos = 0.30){

  df_l <- dplyr::filter(dat, leg == "L")
  df_r <- dplyr::filter(dat, leg == "R")

  lm_l <- lm(abundance ~ week, df_l)
  lm_r <- lm(abundance ~ week, df_r)

  eqn_l <- lm_eqn(lm_l)
  eqn_r <- lm_eqn(lm_r)

  g <- ggplot2::ggplot(dat, aes(x = week, y = abundance)) +
    ggplot2::FUN(mapping = aes(x = week, y = abundance, fill = leg),
        alpha = 0.5, size = 3, pch = 21,  color = "black", width = 0.05) +
    ggplot2::labs(title = g_title, x = "Week", y = "Raw Abundance", fill = "Leg") +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::theme_classic() +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::geom_smooth(mapping = aes(color = leg), method = 'lm', se = FALSE,
                size = 1.1, show.legend = FALSE, linetype = "dotted") +
    ggplot2::annotate("text", x = x_pos, y = (y1_pos)*(max(dat$abundance)),
                      label = eqn_l[1], parse = TRUE, color = "#F98B86") +
    ggplot2::annotate("text", x = x_pos, y = (y1_pos - 0.05)*(max(dat$abundance)),
                      label = eqn_l[2], parse = TRUE, color = "#F98B86") +
    ggplot2::annotate("text", x = x_pos, y = (y1_pos - 0.11)*(max(dat$abundance)),
                      label = eqn_l[3], parse = TRUE, color = "#F98B86") +
    ggplot2::annotate("text", x = x_pos, y = (y2_pos)*(max(dat$abundance)),
                      label = eqn_r[1], parse = TRUE, color = "#53D3D7") +
    ggplot2::annotate("text", x = x_pos, y = (y2_pos - 0.05)*(max(dat$abundance)),
                      label = eqn_r[2], parse = TRUE, color = "#53D3D7") +
    ggplot2::annotate("text", x = x_pos, y = (y2_pos - 0.11)*(max(dat$abundance)),
                      label = eqn_r[3], parse = TRUE, color = "#53D3D7") +
    ggplot2::coord_cartesian(xlim = c(1, 8), clip = 'off') +
    ggplot2::theme(plot.margin = unit(c(1, 5, 1, 1), "lines"))
  return(g)
}
