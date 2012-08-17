\name{imp2state}
\alias{imp2state}
\concept{surmise relation}
\concept{quasi ordinal knowledge space}
\concept{Birkhoff theorem}
\title{
  Transformation from Implications to Knowledge States
}
\description{
  \code{imp2state} transforms a set of implications (ought to be
  a surmise relation) to the corresponding set of knowledge states
  (the quasi ordinal knowledge space).
}
\usage{
imp2state(imp, items)
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
  \code{imp2state} returns a matrix consisting of ones or zeros (the
  quasi ordinal knowledge space), in which each row represents the
  \eqn{1}/\eqn{0}-pattern of a knowledge state.
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
  Anatol Sargin, Ali Uenlue
}
\note{
  For any set of implications the returned knowledge structure is a
  quasi ordinal knowledge space.  In case of a surmise relation this
  is Birkhoff's theorem.  For details refer to \cite{Doignon and
  Falmagne (1999, Theorem 1.49)}.

  A set of implications, an object of the class
  \code{\link[sets]{set}}, consists of \eqn{2}-tuples \eqn{(i, j)} of
  the class \code{\link[sets]{tuple}}, where a \eqn{2}-tuple
  \eqn{(i, j)} is interpreted as `mastering item \eqn{j} implies
  mastering item \eqn{i}.'
}
\seealso{
  \code{\link{state2imp}} for transformation from knowledge states to
  implications.  See also \code{\link{DAKS-package}} for general
  information about this package.
}
\examples{
x <- iita(pisa, v = 1)
imp2state(x$implications, ncol(pisa))
}
\keyword{manip}
\keyword{math}
\keyword{models}
\keyword{utilities}
