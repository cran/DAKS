\name{hasse}
\alias{hasse}
\concept{Hasse diagram}
\title{
  Hasse Diagram of Surmise Relation
}
\description{
  \code{hasse} plots the Hasse diagram of a surmise relation (more
  precisely, of its corresponding quotient set).
}
\usage{
hasse(imp, items)
}
\arguments{
  \item{imp}{a required object of class \code{\link[sets]{set}}
    representing the set of implications, for instance obtained from a
    call to \code{\link{iita}}.}
  \item{items}{a required numeric giving the number of items of the
    domain taken as basis for \code{imp}.}
}
\value{
  If the arguments \code{imp} and \code{items} are of required types,
  \code{hasse} produces a plot, and returns a list of the equally
  informative items.
}
\references{
  Doignon, J.-P. and Falmagne, J.-C. (1999) \emph{Knowledge Spaces}.
  Berlin, Heidelberg, and New York: Springer-Verlag.

  Uenlue, A. and Sargin, A. (2010) \pkg{DAKS}: An \R package for data
  analysis methods in knowledge space theory. \emph{Journal of Statistical
  Software}, \bold{37}(2), 1--31.
  URL \url{http://www.jstatsoft.org/v37/i02/}.
}
\author{
  Anatol Sargin \email{sargin@statistik.tu-dortmund.de},

Ali Uenlue \email{uenlue@statistik.tu-dortmund.de}
}
\note{
  The function \code{hasse} is not capable of plotting equally
  informative items.  This is why equally informative items are
  returned in a list.

  A set of implications, an object of the class
  \code{\link[sets]{set}}, consists of \eqn{2}-tuples \eqn{(i, j)} of
  the class \code{\link[sets]{tuple}}, where a \eqn{2}-tuple
  \eqn{(i, j)} is interpreted as `mastering item \eqn{j} implies
  mastering item \eqn{i}.'
}
\seealso{
  \code{\link{iita}}, the interface that provides the three
  inductive item tree analysis methods under one umbrella. See also
  \code{\link{DAKS-package}} for general information about this
  package.
}
\examples{
## requires the package Rgraphviz from Bioconductor
## users must have installed Graphviz on their computers
\dontrun{
hasse(iita(pisa, v = 2)$implications, 5)
}
}
\keyword{graphs}
\keyword{hplot}
\keyword{print}
