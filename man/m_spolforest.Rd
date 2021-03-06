% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/m_plot_graph.R
\name{m_spolforest}
\alias{m_spolforest}
\title{m_spolforest}
\usage{
m_spolforest(merdata, tree_seed = 1, ...)
}
\arguments{
\item{merdata}{A merdata object}

\item{tree_seed}{A seed for the random number generation.}

\item{\dots}{Additonal plotting parameters passed to the underlying graphing function \code{m_plot_graph}}
}
\value{
A ggplot2 object
}
\description{
Draws a spoligoforest using spoligotypes, incorporating drug resistance status of isolates if present.
}
\examples{
m_spolforest(monteserin)
}
\references{
{Reyes et al. (2008). Models of deletion for visualizing bacterial variation: an application to tuberculosis spoligotypes. BMC Bioinformatics. 2008 Nov 27;9:496. doi: 10.1186/1471-2105-9-496.}

{Tang et al. (2008). spolTools: online utilities for analyzing spoligotypes of the Mycobacterium tuberculosis complex. Bioinformatics. 2008 Oct 15;24(20):2414-5.}
}
\keyword{RES}
\keyword{SPOL}
\keyword{merdata}
\keyword{resistance}
\keyword{spoligotype}
