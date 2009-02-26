\name{pattern}
\alias{pattern}
\concept{response pattern}
\concept{absolute frequency}
\title{Frequencies of Response Patterns and Knowledge States}
\description{
  \code{pattern} computes the absolute frequencies of the (occurring)
  response patterns, and optionally, the absolute frequencies of a
  collection of specified knowledge states in a dataset.
}
\usage{
pattern(dataset, n = 5, P = NULL)
}
\arguments{
  \item{dataset}{a required data frame or matrix consisting of binary,
    \eqn{1} or \eqn{0}, numeric data.}
  \item{n}{an optional numeric, with default \code{n = 5}, giving the
    \eqn{n} highest frequencies and their corresponding response
    patterns to be returned.}
  \item{P}{an optional matrix of ones and zeros giving the knowledge
    states to be used.  The default \code{P = NULL} corresponds to no
    knowledge states being specified.}
}
\details{
  This function can be used to retrieve information about how often
  response patterns and knowledge states occur in a dataset.  The
  argument \code{n} refers to response patterns, not knowledge states,
  and in particular is independent of specifications of the argument
  \code{P}.  If \code{pattern} is called without specifying \eqn{n}
  explicitly, the response patterns with the five highest frequencies
  are returned (along with their frequencies).  If \eqn{n} is
  specified, the response patterns with the \eqn{n} highest
  frequencies are returned (along with their frequencies).  If \eqn{n}
  is larger than the number of different response patterns in the
  dataset, \eqn{n} is set the number of different response patterns.

  The knowledge states are represented as \eqn{1}/\eqn{0}-patterns and
  are the rows of the argument matrix \code{P}.  The matrix \code{P}
  must contain only ones and zeros, which encode whether or not an
  item belongs to a knowledge state, respectively.  If \code{P} is not
  specified, \code{pattern} only returns information about response
  patterns (as described previously).

  The data must contain only ones and zeros, which encode solving or
  failing to solve an item, respectively.
}
\value{
  If the arguments \code{dataset}, \code{n}, and \code{P} are of
  required types, \code{pattern} returns a named list consisting of
  the following two components:

  \item{response.patterns}{an array giving the response patterns (with
    the \eqn{n} highest frequencies) and their absolute frequencies in 
    \code{dataset}.}
  \item{states}{a matrix of the knowledge states and their absolute
    frequencies in \code{dataset}.  Each row represents a knowledge
    state, the last column gives the frequencies of the states.  If
    \code{P = NULL}, the component \code{states} is \code{NULL}.}
  \item{n}{a value specifying the number of states to print.}
}
\references{
  Sargin, A. and Uenlue, A. (2009) Inductive item tree analysis:
  Corrections, improvements, and comparisons.  Manuscript under
  revision.  \url{http://www.math.uni-augsburg.de/~uenlueal/}

  Sargin, A. and Uenlue, A. (2009) \pkg{DAKS}: An \R package for data
  analysis in knowledge space theory. Manuscript submitted for
  publication.  \url{http://www.math.uni-augsburg.de/~uenlueal/}
}
\author{
  Anatol Sargin \email{anatol.sargin@math.uni-augsburg.de},
  Ali Uenlue \email{ali.uenlue@math.uni-augsburg.de}
}
\seealso{
  \code{\link{ob_counter}} for computation of numbers of
  counterexamples; \code{\link{simu}} for data simulation tool;
  \code{\link{iita}}, the interface that provides the three inductive
  item tree analysis methods under one umbrella.  See also
  \code{\link{DAKS-package}} for general information about this
  package.
}
\examples{
pattern(pisa, n = 3)
pattern(pisa)
}
\keyword{manip}
\keyword{univar}
\keyword{utilities}
