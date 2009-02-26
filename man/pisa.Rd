\name{pisa}
\alias{pisa}
\docType{data}
\title{Programme for International Student Assessment (PISA) Data}
\description{
  The accompanying binary dataset is part of the empirical 2003
  Programme for International Student Assessment (PISA) data.  It
  contains the item responses by \eqn{340} German students on a
  \eqn{5}-item dichotomously scored mathematical literacy test.
}
\usage{
pisa
}
\format{
  The \code{pisa} data frame consists of \eqn{340} rows and \eqn{5}
  columns, representing the response patterns of the students to the
  test items.  Each number, an integer, \eqn{1} or \eqn{0}, encodes a
  correct or incorrect response, respectively.
}
\note{
  The dataset \code{pisa} was obtained after dichotomizing the
  original multiple-choice or open format test data.  Wording of the
  test items used in the assessment is not known (not available
  publicly).
}
\source{
  OECD Programme for International Student Assessment (PISA).
  \url{http://www.pisa.oecd.org/}
}
\references{
  Sargin, A. and Uenlue, A. (2009) Inductive item tree analysis:
  Corrections, improvements, and comparisons.  Manuscript under
  revision.  \url{http://www.math.uni-augsburg.de/~uenlueal/}

  Sargin, A. and Uenlue, A. (2009) \pkg{DAKS}: An \R package for data
  analysis in knowledge space theory. Manuscript submitted for
  publication.  \url{http://www.math.uni-augsburg.de/~uenlueal/}
}
\seealso{
  See \code{\link{DAKS-package}} for general information about this
  package.
}
\keyword{datasets}
