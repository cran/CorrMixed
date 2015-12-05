\name{Heatmap}
\alias{Heatmap}
\title{
Plot a heatmap of the correlation structure 
}
\description{
This function plots a heatmap of the correlation structure (reliability) in the data. It is a wrapper function for the \code{cor.plot} function of the \code{psych} package.}
\usage{
Heatmap(Dataset, Id, Outcome, Time, ...)
}
\arguments{
  \item{Dataset}{A \code{data.frame} that should consist of multiple lines per subject ('long' format).}
  \item{Id}{The subject indicator.}
  \item{Outcome}{The outcome indicator.}
  \item{Time}{The time indicator.}
  \item{...}{Other arguments to be passed to \code{cor.plot}.}
}
\references{
Van der Elst, W., Molenberghs, G., Hilgers, R., & Heussen, N. (2015). Estimating the reliability of repeatedly measured endpoints based on linear mixed-effects models. A tutorial. \emph{Submitted.}
}
\author{
Wim Van der Elst, Geert Molenberghs, Ralf-Dieter Hilgers, & Nicole Heussen
}
\seealso{
\code{\link{plot.Explore.WS.Corr}}
}
\examples{
# Open data
data(Example.Data)

# Make heatmap
Heatmap(Dataset=Example.Data, Id = "Id", 
Outcome="Outcome", Time = "Time")

# Make heatmap in black and white
Heatmap(Dataset=Example.Data, Id = "Id", 
Outcome="Outcome", Time = "Time", colors=FALSE)
}
\keyword{Exploratory analysis}
\keyword{Within-subject correlation}
\keyword{Heatmap}
\keyword{Plot}
\keyword{Reliability}