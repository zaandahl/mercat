% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/m_utilities.R
\name{m_hex2bin}
\alias{m_hex2bin}
\title{m_hex2bin}
\usage{
m_hex2bin(hex)
}
\arguments{
\item{hex}{A spoligotype pattern in hex format}
}
\value{
A spoligotype pattern in binary format
}
\description{
Converts a hex spoligotype pattern to binary format. 
For more information see Dale, J. W., et al. "Spacer oligonucleotide typing 
of bacteria of the Mycobacterium tuberculosis complex: recommendations for 
standardised nomenclature [The Language of Our Science]." The International 
Journal of Tuberculosis and Lung Disease 5.3 (2001): 216-219.
}
\examples{
m_hex2bin("7F-7E-1E-FF-F0-FF")
}
\keyword{format}
\keyword{hex}
\keyword{spoligotype}
