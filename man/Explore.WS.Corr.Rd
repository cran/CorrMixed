\name{Explore.WS.Corr}
\alias{Explore.WS.Corr}
\title{
Explore within-subject correlations (reliabilities)
}
\description{
This function allows for exploring the  within-subject (test-retest) correlation (\eqn{R}) structure in the data, taking relevant covariates into account. Estimated correlations as a function of time lag (= absolute difference between measurement moments \eqn{t_1} and \eqn{t_2}) are provided as well as their confidence intervals (based on a non-parametric bootstrap). 
}
\usage{
Explore.WS.Corr(OLS.Model=" ", Dataset, Id, Time, 
Alpha=0.05, Smoother.Span=.2, Number.Bootstrap=100, 
Seed=1)
}
\arguments{
  \item{OLS.Model}{\code{OLS.Model} is a formula passed to \code{lm} (to obtain the OLS residuals, i.e., to take covariates into account in the computation of \eqn{R}). \code{OLS.Model} should thus be a formula that specifies the outcome of interest followed by a ~ sign and the covariates to be taken into account, e.g. 
  
    \code{OLS.Model="Outcome~1+as.factor(Time) + as.factor(Treatment)"}. }
  \item{Dataset}{A \code{data.frame} that should consist of multiple lines per subject ('long' format).}
  \item{Id}{The subject indicator.}
  \item{Time}{The time indicator. Should be coded as \eqn{1}, \eqn{2}, etc.}
\item{Alpha}{The \eqn{\alpha}-level to be used in the non-parametric bootstrap-based Confidence Interval for \eqn{R}. Default \code{Alpha=0.05}} 
\item{Smoother.Span}{A smoothing (loess) technique is used to estimate \eqn{R} as a function of time lag. The smoother span gives the proportion of points in the plot which influence the smooth at each value. Larger values give more smoothness. For details, see \url{https://stat.ethz.ch/R-manual/R-patched/library/stats/html/lowess.html}. Defauls \code{Smoother.Span=.2}.}
\item{Number.Bootstrap}{The number of non-parametric bootstrap samples to be used to estimate the Confidence Interval for \eqn{R}. Default \code{Number.Bootstrap=100}} 
\item{Seed}{The seed to be used in the bootstrap. Default \code{Seed=1}.}
}
\value{
\item{Est.Corr}{The estimated correlations \eqn{R} as a function of time lag. A smoothing (loess) technique is used to estimate \eqn{R} as a function of time lag (based on the output in \code{All.Corrs).}}
\item{All.Corrs}{A \code{matrix} that contains the estimated correlations \eqn{R} for all individual time lags.}
\item{Bootstrapped.Corrs}{A \code{matrix} that contains the estimated correlations \eqn{R} as a function of time lag in the bootstrapped samples.}
\item{Alpha}{The \eqn{\alpha} level used in the estimation of the confidence interval.}
\item{CI.Upper}{The upper bounds of the confidence intervals.}
\item{CI.Lower}{The lower bounds of the confidence intervals.}
}
\references{
Van der Elst, W., Molenberghs, G., Hilgers, R., & Heussen, N. (2015). Correlation in continuous monitoring of vital parameters - estimating reliability using linear mixed-effects models. \emph{Submitted.}
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

# Explore correlation structure
Expl_Corr <- Explore.WS.Corr(OLS.Model="Outcome~as.factor(Time)+ 
  as.factor(Cycle) + as.factor(Condition)", Dataset=Example.Data, 
  Id="Id", Time="Time", Alpha=.05, Number.Bootstrap=50, Seed=123)

# explore results
summary(Expl_Corr)

# plot with correlations for all time lags, and 
# add smoothed (loess) correlation function 
plot(Expl_Corr, Indiv.Corrs=TRUE)
# plot bootstrapped smoothed (loess) correlation function 
plot(Expl_Corr)
}
\keyword{Exploratory analysis}
\keyword{Within-subject correlation}
\keyword{Bootstrap}
\keyword{Reliability}
