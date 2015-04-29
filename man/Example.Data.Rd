\name{Example.Data}
\alias{Example.Data}
\docType{data}
\title{
An example dataset
}
\description{
\code{Example.Data} is a hypothetical dataset constructed to demonstrate some of the functions in the package. Data are provided for a hypothetical experiment in which a stimulus is provided under different experimental conditions. The outcome is a normally distributed variable. The entire experiment is repeated multiple times (cycle) in each patient.}
\usage{data(Example.Data)}
\format{
A \code{data.frame} with \eqn{360} observations on \eqn{5} variables.
  \describe{
    \item{\code{Id}}{The Subject identifier.}
    \item{\code{Cycle}}{The same experiment is repeated multiple times in a patient. Cycle indicates the order of these repeated experiments.}
    \item{\code{Condition}}{The experimental condition under which the outcome was measured.}
    \item{\code{Time}}{The time point at which the outcome was measured.}
    \item{\code{Outcome}}{A continuous outcome.}   
  }
}
\keyword{Example.Data}
