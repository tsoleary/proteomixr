% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pep_iso.R
\name{pep_iso}
\alias{pep_iso}
\title{Peptide Isotopic Distribution}
\usage{
pep_iso(pep, max_iso = 9, charge = 1)
}
\arguments{
\item{pep}{peptide}

\item{max_iso}{number of isotopes to calculate}

\item{charge}{charge of the peptide in MS data}
}
\description{
Returns a data frame with the relative abundances of each isotope in a peptide
}
\section{Warning}{

Does not yet have capability to handle modified peptides (e.g. oxidation)
}

\examples{
pep_iso("SAMPLER", max_iso = 6, charge = 2)
}
\keyword{distribution}
\keyword{isotopic}
\keyword{peptide}
