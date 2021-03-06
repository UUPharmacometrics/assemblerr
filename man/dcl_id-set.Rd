% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/declaration.R
\name{dcl_id<-}
\alias{dcl_id<-}
\title{Set declaration fields}
\usage{
dcl_id(dcl) <- value
}
\arguments{
\item{dcl}{A declaration}

\item{value}{An expression or a list of expressions}
}
\value{
The modified declaration
}
\description{
Set declaration fields
}
\examples{
d <- declaration(cl~theta*exp(eta))
assemblerr:::dcl_id(d) <- quote(v)
}
\keyword{internal}
