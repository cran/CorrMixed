\name{plot Explore.WS.Corr}
\alias{plot.Explore.WS.Corr}
\alias{plot Explore.WS.Corr}
\title{
Plot of exploratory within-subject correlations (reliabilities) 
}
\description{
Provides an exploratory plot that allows for examining the within-subject correlations \eqn{R} (reliabilities) as a function if time lag.
}
\usage{
\method{plot}{Explore.WS.Corr}(x, Est.Corrs=TRUE, Indiv.Corrs=FALSE, 
Add.CI=FALSE, Add.CI.Smoothed=TRUE, Smoother.Span=0.2, 
Add.Boot.Corrs=FALSE, Add.CI.Polygon=FALSE, 
ylim=c(-1, 1), xlab="Time Lag", ylab="Reliability", ...)
}
\arguments{
  \item{x}{A fitted object of class \code{Explore.WS.Corr}.}
  \item{Est.Corrs}{Logical. Should the smoothed (loess) correlation function as a function of time lag be added? Default \code{TRUE}.}
  \item{Indiv.Corrs}{Logical. Should the estimated correlations for all individual time lags be added? Default \code{FALSE}.}
  \item{Add.CI}{Logical. Should a bootstrapped \eqn{100(1-\alpha)}\% Confidence Interval be added around the smoothed correlation function? Default \code{FALSE}.}
  \item{Add.CI.Smoothed}{Logical. Should a smoothed bootstrapped \eqn{100(1-\alpha)}\% Confidence Interval be added around the smoothed correlation function? Default \code{FALSE}.}  
  \item{Smoother.Span}{The smoother span to be used. The smoother span gives the proportion of points in the plot which influence the smooth at each value. Larger values give more smoothness. For details, see \url{https://stat.ethz.ch/R-manual/R-patched/library/stats/html/lowess.html}. Defauls \code{Smoother.Span=.2}.}
  \item{Add.Boot.Corrs}{Logical. Should the inidividual bootstrapped smoothed (loess) correlation functions be added? Default \code{FALSE}.}  
  \item{Add.CI.Polygon}{Logical. Similar to \code{Add.CI} but adds a grey polygon to mark the a bootstrapped \eqn{100(1-\alpha)}\% Confidence Interval (instead of dashed lines). Default \code{FALSE}.}
  \item{ylim}{The minimum and maximum values of the Y-axis. Default \code{ylim=c(-1,1)}.}
  \item{xlab}{The label of the X-axis. Default \code{xlab="Time Lag"}.}
  \item{ylab}{The label of the Y-axis. Default \code{ylab="Reliability"}.}
  \item{...}{Other arguments to be passed to the plot function.}
}
\references{
Van der Elst, W., Molenberghs, G., Hilgers, R., & Heussen, N. (2015). Correlation in continuous monitoring of vital parameters I - estimating reliability using linear mixed-effects models. \emph{Submitted.}
}
\author{
Wim Van der Elst, Geert Molenberghs, Ralf-Dieter Hilgers, & Nicole Heussen
}
\seealso{
\code{\link{Explore.WS.Corr}, \link{Heatmap}}
}
\examples{
# Open data
data(Example.Data)

# Explore correlation structure
Expl_Corr <- Explore.WS.Corr(OLS.Model="Outcome~as.factor(Time)+ 
  as.factor(Cycle) + as.factor(Condition)", Dataset=Example.Data, 
  Id="Id", Time="Time", Max.Time=40, Alpha=.05, Number.Bootstrap=50, Seed=123)

# explore results
summary(Expl_Corr)

# plot with correlations for all time lags, and 
# add smoothed (loess) correlation function 
plot(Expl_Corr, Indiv.Corrs=TRUE, Add.CI=FALSE, Add.Boot.Corrs=FALSE)
# plot bootstrapped smoothed (loess) correlation function 
plot(Expl_Corr, Add.Boot.Corrs=TRUE)
}
\keyword{Exploratory analysis}
\keyword{Within-subject correlation}
\keyword{Bootstrap}
\keyword{Plot}
\keyword{Reliability}