\name{plot.WS.Corr.Mixed}
\alias{plot.WS.Corr.Mixed}
\alias{plot.WS.Corr.Mixed.SAS}
\alias{plot WS.Corr.Mixed}
\alias{plot WS.Corr.Mixed.SAS}
\title{
Plot the within-subject correlations (reliabilities) obtained by using the mixed-effects modeling approch
}
\description{
Plots the within-subject correlations (reliabilities) and \eqn{100(1-\alpha)}\% Confidence Intervals based on the fitted mixed-effect models.
}
\usage{
\method{plot}{WS.Corr.Mixed}(x, xlab, ylab, ylim, main, All.Individual=FALSE, ...)
}
\arguments{
  \item{x}{A fitted object of class \code{WS.Corr.Mixed}}
  \item{xlab}{The label of the X-axis.}
  \item{ylab}{The label of the Y-axis.}
  \item{ylim}{The min, max values of the Y-axis.}
  \item{main}{The main title of the plot.}
  \item{All.Individual}{\code{Logical}. Should correlation functions be provided that show the correlations between all indidual measurement moments \eqn{R(t_{i},t_{k})}? Argument is only used if Model \eqn{2} was fitted. Default \code{All.Individual=FALSE}.}
  \item{...}{Other arguments to be passed to the plot function.}
}
\references{
Van der Elst, W., Molenberghs, G., Hilgers, R., & Heussen, N. (2015). Correlation in continuous monitoring of vital parameters - estimating reliability using linear mixed-effects models. \emph{Submitted.}
}
\author{
Wim Van der Elst, Geert Molenberghs, Ralf-Dieter Hilgers, & Nicole Heussen
}
\seealso{
\code{\link{WS.Corr.Mixed}}, \code{\link{plot WS.Corr.Mixed}} 
}
\examples{
# open data
data(Example.Data)

# Make covariates used in mixed model
Example.Data$Time2 <- Example.Data$Time**2
Example.Data$Time3 <- Example.Data$Time**3
Example.Data$Time3_log <- (Example.Data$Time**3) * (log(Example.Data$Time))

# model 1: random intercept model
Model1 <- WS.Corr.Mixed(
Fixed.Part=Outcome ~ Time2 + Time3 + Time3_log + as.factor(Cycle) 
+ as.factor(Condition), Random.Part = ~ 1|Id, 
Dataset=Example.Data, Model=1, Id="Id", Number.Bootstrap = 50, 
Seed = 12345)

  # plot the results
plot(Model1)

\dontrun{time-consuming code parts
# model 2: random intercept + Gaussian serial corr
Model2 <- WS.Corr.Mixed(
Fixed.Part=Outcome ~ Time2 + Time3 + Time3_log + as.factor(Cycle) 
+ as.factor(Condition), Random.Part = ~ 1|Id, 
Correlation=corGaus(form= ~ Time, nugget = TRUE),
Dataset=Example.Data, Model=2, Id="Id", Seed = 12345)

  # plot the results
    # estimated corrs as a function of time lag (default plot)
plot(Model2)
    # estimated corrs for all pairs of time points
plot(Model2, All.Individual = T)

# model 3
Model3 <- WS.Corr.Mixed(
  Fixed.Part=Outcome ~ Time2 + Time3 + Time3_log + as.factor(Cycle) 
  + as.factor(Condition), Random.Part = ~ 1 + Time|Id, 
  Correlation=corGaus(form= ~ Time, nugget = TRUE),
  Dataset=Example.Data, Model=3, Id="Id", Seed = 12345)

  # plot the results
    # estimated corrs for all pairs of time points
plot(Model3)
    # estimated corrs as a function of time lag
}
}
\keyword{Within-subject correlation}
\keyword{Test-retest correlation}
\keyword{Mixed-effects model}
\keyword{Bootstrap}
\keyword{Reliability}