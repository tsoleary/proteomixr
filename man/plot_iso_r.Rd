% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_iso_r.R
\name{plot_iso_r}
\alias{plot_iso_r}
\title{Plot Isotopic Ratio}
\usage{
plot_iso_r(dat, g_title, g_subtitle, FUN = geom_jitter, x_pos = 3.35,
  y_pos1 = 0.83, y_pos2 = 0.3)
}
\arguments{
\item{dat}{a data frame with ratio and week columns}

\item{g_title}{title of the graph}

\item{g_subtitle}{subtitle of the graph}

\item{FUN}{plotting function, geom_jitter or geom_point}

\item{x_pos, y_pos1, y_pos2}{x and y positions of the k and plateau values}
}
\description{
Plots the changing corrected isotopic ratio over time
}
\examples{
plot_iso_r(df_g, g_title = "Protein", g_subtitle = "PEPTIDE")
}
\keyword{corrected}
\keyword{isotopic}
\keyword{plot}
\keyword{ratio,}
