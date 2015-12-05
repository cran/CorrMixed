\name{Spaghetti.Plot}
\alias{Spaghetti.Plot}
\title{Make a Spaghetti plot}
\description{
Makes a spaghetti plot, i.e., a plot that depicts the outcome as a function of time for each individual subject.  
}
\usage{
Spaghetti.Plot(Dataset, Outcome, Time, Id, Add.Profiles=TRUE, Add.Mean=TRUE, 
Add.Median=FALSE, Col=8, Lwd.Me=3, xlim, ylim, ...)
}
\arguments{
  \item{Dataset}{A \code{data.frame} that should consist of multiple lines per subject ('long' format).}
  \item{Outcome}{The name of the outcome variable.}
  \item{Time}{The name of the time indicator.}
  \item{Id}{The subject indicator.}
  \item{Add.Profiles}{Logical. Should the individual profiles be added? Default \code{Add.Profiles=TRUE}.}
  \item{Add.Mean}{Logical. Should a line that depicts the mean as a function of time be added? Default \code{Add.Mean=TRUE}.}
  \item{Add.Median}{Logical. Should a line that depicts the medean as a function of time be added? Default \code{Add.Mean=FALSE}.}
  \item{Col}{The color of the individual profiles. Default \code{Col=8} (grey).}
  \item{Lwd.Me}{The line width of the lines with mean and/or median. Default \code{Lwd.Me=3}.}
  \item{xlim}{The (min, max) values for the x-axis.}
  \item{ylim}{The (min, max) values for the y-axis.}
  \item{...}{Other arguments to be passed to the \code{plot()} function.}
}
\references{
Van der Elst, W., Molenberghs, G., Hilgers, R., & Heussen, N. (2015). Estimating the reliability of repeatedly measured endpoints based on linear mixed-effects models. A tutorial. \emph{Submitted.}
}
\author{
Wim Van der Elst, Geert Molenberghs, Ralf-Dieter Hilgers, & Nicole Heussen
}
\examples{
# Open data
data(Example.Data)

# Plot individual profiles + mean
Spaghetti.Plot(Dataset = Example.Data, Outcome = Outcome, Id=Id, Time = Time)

# Plot individual profiles + median
Spaghetti.Plot(Dataset = Example.Data, Outcome = Outcome, Id=Id, Time = Time,
Add.Mean = FALSE, Add.Median = TRUE)
}
\keyword{Exploratory analysis}
\keyword{Individual profiles}
\keyword{Mean structure}