% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/declaration.R
\name{dcl_add}
\alias{dcl_add}
\alias{dcl_substract}
\alias{dcl_multiply}
\alias{dcl_devide}
\title{Arithmetically combine declarations}
\usage{
dcl_add(dcl1, dcl2, lhs = dcl_id(dcl1))

dcl_substract(dcl1, dcl2, lhs = dcl_id(dcl1))

dcl_multiply(dcl1, dcl2, lhs = dcl_id(dcl1))

dcl_devide(dcl1, dcl2, lhs = dcl_id(dcl1))
}
\arguments{
\item{dcl1}{A declaration}

\item{dcl2}{A declaration}

\item{lhs}{List of expressions for the left-hand side of the resulting declaration}
}
\value{
A declaration
}
\description{
These functions allow to combine two declarations using addition, substraction, multiplication, or division.
}
\section{Functions}{
\itemize{
\item \code{dcl_add()}: Addition of the declarations

\item \code{dcl_substract()}: Substraction of the declarations

\item \code{dcl_multiply()}: Multiplications of the declarations

\item \code{dcl_devide()}: Division of the declarations

}}
\keyword{internal}
