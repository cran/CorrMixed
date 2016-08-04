\name{WS.Corr.Mixed}
\alias{WS.Corr.Mixed}
\title{
Estimate within-subject correlations (reliabilities) based on a mixed-effects model.
}
\description{
This function allows for the estimation of the within-subject correlations using a general and flexible modeling approach that allows at the same time to capture hierarchies in the data, the presence of covariates, and the derivation of correlation estimates. Non-parametric bootstrap-based confidence intervals can be requested. }
\usage{
WS.Corr.Mixed(Dataset, Fixed.Part=" ", Random.Part=" ", 
Correlation=" ", Id, Time=Time, Model=1, 
Number.Bootstrap=100, Alpha=.05, Seed=1)
}
\arguments{
  \item{Dataset}{A \code{data.frame} that should consist of multiple lines per subject ('long' format).}
  \item{Fixed.Part}{The outcome and fixed-effect part of the mixed-effects model to be fitted. The model should be specified in agreement with the \code{lme} function requirements of the \code{nlme} package. See \bold{examples} below.}
  \item{Random.Part}{The random-effect part of the mixed-effects model to be fitted (specified in line with the \code{lme} function requirements). See \bold{examples} below.}
  \item{Correlation}{An optional object describing the within-group correlation structure (specified in line with the \code{lme} function requirements). See \bold{examples} below.} 
  \item{Id}{The subject indicator.}
  \item{Time}{The time indicator. Default \code{Time=Time}.}
  \item{Model}{The type of model that should be fitted. \code{Model=1}: random intercept model, \code{Model=2}: random intercept and Gaussian serial correlation, and \code{Model=3}: random intercept,  slope, and Gaussian serial correlation. Default \code{Model=1}.}
   \item{Number.Bootstrap}{The number of bootstrap samples to be used to estimate the Confidence Intervals around \eqn{R}.  Default \code{Number.Bootstrap=100}. As an alternative to obtain confidence intervals, the Delta method can be used (see \link{WS.Corr.Mixed.SAS}). }
   \item{Alpha}{The \eqn{\alpha}-level to be used in the bootstrap-based Confidence Interval for \eqn{R}. Default \eqn{Alpha=0.05}} 
\item{Seed}{The seed to be used in the bootstrap. Default \eqn{Seed=1}.}
}
\details{
\bold{Warning 1}

To avoid problems with the \code{lme} function, do not specify powers directly in the function call. For example, rather than specifying \code{Fixed.Part=ZSV ~ Time + Time**2} in the function call, first add \code{Time**2} to the dataset 
(\code{Dataset$TimeSq <- Dataset$Time ** 2}) and then use the new variable name in the call:
\code{Fixed.Part=ZSV ~ Time + TimeSq}


\bold{Warning 2}
To avoid problems with the \code{lme} function, specify the Random.Part and Correlation arguments like e.g.,
\code{Random.Part = ~ 1| Subject} and 
\code{Correlation=corGaus(form= ~ Time, nugget = TRUE)}

not like e.g., 
\code{Random.Part = ~ 1| Subject} and
\code{Correlation=corGaus(form= ~ Time| Subject, nugget = TRUE)} 
  
(i.e., do not use \code{Time| Subject})

}
\value{
\item{Model}{The type of model that was fitted (model \eqn{1}, \eqn{2}, or \eqn{3}.)}
\item{D}{The \eqn{D} matrix of the fitted model.}
\item{Tau2}{The \eqn{\tau^2} component of the fitted model. This component is only obtained when serial correlation is requested (Model \eqn{2} or \eqn{3}), \eqn{\varepsilon_{2}  \sim N(0, \tau^2 H_{i}))}.}
\item{Rho}{The \eqn{\rho} component of the fitted model which determines the matrix \eqn{H_{i}}, \eqn{\rho(|t_{ij}-t_{ik}|)}. This component is only obtained when serial correlation is considered (Model \eqn{2} or \eqn{3}).}
\item{Sigma2}{The residual variance.}
\item{AIC}{The AIC value of the fitted model.}
\item{LogLik}{The log likelihood value of the fitted model.}
\item{R}{The estimated reliabilities.}
\item{CI.Upper}{The upper bounds of the bootstrapped confidence intervals.}
\item{CI.Lower}{The lower bounds of the bootstrapped confidence intervals.}
\item{Alpha}{The \eqn{\alpha} level used in the estimation of the confidence interval.}
\item{Coef.Fixed}{The estimated fixed-effect parameters.}
\item{Std.Error.Fixed}{The standard errors of the fixed-effect parameters.}
\item{Time}{The time values in the dataset.}
\item{Fitted.Model}{A fitted model of class \code{lme}.}
}
\references{
Van der Elst, W., Molenberghs, G., Hilgers, R., & Heussen, N. (2015). Estimating the reliability of repeatedly measured endpoints based on linear mixed-effects models. A tutorial. \emph{Submitted.}
}
\author{
Wim Van der Elst, Geert Molenberghs, Ralf-Dieter Hilgers, & Nicole Heussen
}
\seealso{
\code{\link{Explore.WS.Corr}, \link{WS.Corr.Mixed.SAS}} 
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

  # summary of the results
summary(Model1)
  # plot the results
plot(Model1)

\dontrun{time-consuming code parts
# model 2: random intercept + Gaussian serial corr
Model2 <- WS.Corr.Mixed(
Fixed.Part=Outcome ~ Time2 + Time3 + Time3_log + as.factor(Cycle) 
+ as.factor(Condition), Random.Part = ~ 1|Id, 
Correlation=corGaus(form= ~ Time, nugget = TRUE),
Dataset=Example.Data, Model=2, Id="Id", Seed = 12345)

  # summary of the results
summary(Model2)

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

  # summary of the results
summary(Model3)

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