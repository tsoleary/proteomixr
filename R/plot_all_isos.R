#' Plot the Isotopic Distribution Changing Over Time
#'
#' Creates a vector containing the abundance ratio for each peptide
#' @param dat a data frame containing isotope and abundance
#' @param g_title title for the plot
#' @param g_subtitle subtitle for the plot
#' @param FUN geom_jitter or geom_point (defaults to geom_jitter)
#' @keywords plot, isotopic distribution
#' @export
#' @examples
#' df_g <- filter(df_iso, protein == "Hist2h4", peptide == "DAVTYTEHAK")
#' plot_all_isos(df_g, "PRO", "PEP")

plot_all_isos <- function(dat, g_title, g_subtitle, FUN = geom_jitter){
  g <- ggplot(dat, aes(x = isotope, y = abundance)) +
    FUN(mapping = aes(x = isotope, y = abundance, fill = week),
        alpha = 0.5, size = 3, pch = 21, color = "black", width = 0.01) +
    scale_fill_brewer(palette = "Spectral") +
    labs(title = g_title, subtitle = g_subtitle,
         x = "Isotopomer", y = "Abundance\n(% Total)",
         fill = "Week") +
    theme_bw() +
    facet_wrap( ~ dat$leg) +
    theme(strip.background = element_rect(color = "black", fill = "#53D3D7"))
  return(g)
}
