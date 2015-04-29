\name{WS.Corr.Mixed.SAS}
\alias{WS.Corr.Mixed.SAS}
\title{
Estimate within-subject (test-retest) correlations based on a mixed-effects model using the SAS proc MIXED output.
}
\description{
This function allows for the estimation of the within-subject correlations using a general and flexible modeling approach that allows at the same time to capture hierarchies in the data, the presence of covariates, and the derivation of correlation estimates. The output of proc MIXED (SAS) is used as the input for this function. Confidence intervals for the correlations based on the Delta method are provided.
}
\usage{
WS.Corr.Mixed.SAS(Model, D, Sigma2, Asycov, Rho, Tau2, Alpha=0.05, Max.Time)
}
\arguments{
  \item{Model}{The type of model that should be fitted. \code{Model=1}: random intercept model, \code{Model=2}: random intercept and serial correlation, and \code{Model=3}: random intercept,  slope, and serial correlation. Default \code{Model=1}.}
\item{D}{The \eqn{D} matrix of the fitted model.}
\item{Sigma2}{The residual variance.}
\item{Asycov}{The asymptotic correlation matrix of covariance parameter estimates.}
\item{Rho}{The \eqn{\rho} component of the fitted model which determines the matrix \eqn{H_{i}}, \eqn{\rho(|t_{ij}-t_{ik}|)}. This component is only needed when serial correlation is involved, i.e., when Model \eqn{2} or \eqn{3} used.}
\item{Tau2}{The \eqn{\tau^2} component of the fitted model. This component is only needed when serial correlation is involved (i.e., when Model \eqn{2} or \eqn{3} used), \eqn{\varepsilon_{2} \sim N(0, \tau^2 H_{i}))}.}
\item{Alpha}{The \eqn{\alpha}-level to be used in the computation of the Confidence Intervals around the within-subject correlation. The Confidence Intervals are based on the Delta method. Default \code{Alpha=0.05}.}
   \item{Max.Time}{The maximum time moment for which the estimated correlations should be provided.}
}
\value{
\item{Model}{The type of model that was fitted.}
\item{R}{The estimated within-subject correlations.}
\item{Alpha}{The \eqn{\alpha}-level used to computed the Confidence Intervals around \eqn{R}.}
\item{CI.Upper}{The upper bounds of the confidence intervals (Delta-method based).}
\item{CI.Lower}{The lower bounds of the confidence intervals (Delta-method based).}
\item{Max.Time}{The maximum time point for which \eqn{R} was computed.}
}
\references{
Van der Elst, W., Molenberghs, G., Hilgers, R., & Heussen, N. (2015). Correlation in continuous monitoring of vital parameters I - estimating reliability using linear mixed-effects models. \emph{Submitted.}
}
\author{
Wim Van der Elst, Geert Molenberghs, Ralf-Dieter Hilgers, & Nicole Heussen
}
\seealso{
\code{\link{WS.Corr.Mixed}}
}
\examples{
# Open data 
data(Example.Data)

# Estimate R and Delta method-based CI 
# based on SAS output of fitted Model 2

# First specify asycov matrix
Asy_mat <- matrix(c(129170, -10248, -12.0814, -74.8605,
                    -10248, 25894, 21.0976, -50.1059,
                    -12.0814, 21.0976, 0.07791, 1.2120,
                    -74.8605, -50.1059, 1.212, 370.65), nrow = 4)
Model2_SAS <-  WS.Corr.Mixed.SAS(Model="Model 2", 
D=500.98, Tau2=892.97, Rho=3.6302, Sigma2=190.09, 
Asycov = Asy_mat, Max.Time=45)                               
summary(Model2_SAS)
plot(Model2_SAS)
}
\keyword{Within-subject correlation}
\keyword{Test-retest correlation}
\keyword{Mixed-effects model}
\keyword{Bootstrap}
\keyword{SAS}
\keyword{Delta-method based CI}
\keyword{Reliability}
