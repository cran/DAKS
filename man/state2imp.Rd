\name{state2imp}
\alias{state2imp}
\concept{surmise relation}
\concept{quasi ordinal knowledge space}
\concept{Birkhoff theorem}
\title{
  Transformation from Knowledge States to Implications
}
\description{
  \code{state2imp} transforms a set of knowledge states (ought to
  be a quasi ordinal knowledge space) to the corresponding set of
  implications (the surmise relation).
}
\usage{
state2imp(P)
}
\arguments{
  \item{P}{a required matrix of ones and zeros giving the knowledge
    states to be used.  Each row represents the
    \eqn{1}/\eqn{0}-pattern of a knowledge state.}
}
\value{
  If the argument \code{P} is of required type, \code{state2imp}
  returns an object of class \code{\link[sets]{set}} (the surmise
  relation) representing the set of implications.
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
  For any set of knowledge states the returned binary relation is a
  surmise relation.  In case of a quasi ordinal knowledge space this
  is Birkhoff's theorem.  For details refer to \cite{Doignon and
  Falmagne (1999, Theorem 1.49)}.

  A set of implications, an object of the class
  \code{\link[sets]{set}}, consists of \eqn{2}-tuples \eqn{(i, j)} of
  the class \code{\link[sets]{tuple}}, where a \eqn{2}-tuple
  \eqn{(i, j)} is interpreted as `mastering item \eqn{j} implies
  mastering item \eqn{i}.'
}
\seealso{
  \code{\link{imp2state}} for transformation from implications to
  knowledge states.  See also \code{\link{DAKS-package}} for general
  information about this package.
}
\examples{
## an arbitrary matrix of knowledge states is defined
x <- matrix(0, nrow = 5, ncol = 3)
x[1, ] <- c(0, 0, 0)
x[2, ] <- c(0, 0, 1)
x[3, ] <- c(0, 1, 0)
x[4, ] <- c(0, 1, 1)
x[5, ] <- c(1, 1, 1)

state2imp(x)
}
\keyword{manip}
\keyword{math}
\keyword{models}
\keyword{utilities}
