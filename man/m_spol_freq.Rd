% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/m_plot.R
\name{m_spol_freq}
\alias{m_spol_freq}
\title{m_spol_freq}
\usage{
m_spol_freq(merdata, title = "Spoligotype frequency", title_size = 16,
  xaxis_size = 12, yaxis_size = 12, label_size = 12,
  summarylab = "Summary", xlab = "Spacer", ylab = "Count",
  col_high = "black", col_low = "white")
}
\arguments{
\item{merdata}{A merdata object}

\item{title}{the title of the graph}

\item{title_size}{the font size of the title}

\item{xaxis_size}{the font size of the X-axis}

\item{yaxis_size}{the font size of the Y-axis}

\item{label_size}{the font size for labels}

\item{summarylab}{the label of the last row}

\item{xlab}{the X-axis label}

\item{ylab}{the Y-axis label}

\item{col_high}{colour for spacer presence}

\item{col_low}{colour for spacer absence}
}
\description{
Displays the spoligotype patterns in a data set. 
Black represents the presence and white the absence of a spacer. 
The spacer frequencies are summarised in a heat plot at the bottom.
}
\examples{
m_spol_freq(monteserin)
}
\keyword{SPOL}
\keyword{merdata}
\keyword{spoligotype}
