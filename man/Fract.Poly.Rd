\name{Fract.Poly}
\alias{Fract.Poly}
\title{Fit fractional polynomials}
\description{
Fits regression models with \eqn{m} terms of the form \eqn{X^{p}}, where the exponents \eqn{p} are selected from a small predefined set \eqn{S} of both integer and non-integer values. 
}
\usage{
Fract.Poly(Covariate, Outcome, S=c(-2,-1,-0.5,0,0.5,1,2,3), Max.M=5, Dataset)
}
\arguments{
  \item{Covariate}{The covariate to be considered in the models.}
  \item{Outcome}{The outcome to be considered in the models.}
  \item{S}{The set \eqn{S} from which each power \eqn{p^{m}}
  is selected. Default 
  \code{S={-2, -1, -0.5, 0, 0.5, 1, 2, 3}}.}
  \item{Max.M}{The maximum order \eqn{M} to be considered for the fractional polynomial. This value can be \eqn{5} at most. When \eqn{M=5}, then fractional polynomials of order \eqn{1} to \eqn{5} are considered. Default \code{Max.M=5}.}
  \item{Dataset}{A \code{data.frame} that should consist of multiple lines per subject ('long' format).}
}
\value{
\item{Results.M1}{The results (powers and AIC values) of the fractional polynomials of order \eqn{1}.}
\item{Results.M2}{The results (powers and AIC values) of the fractional polynomials of order \eqn{2}.}
\item{Results.M3}{The results (powers and AIC values) of the fractional polynomials of order \eqn{3}.}
\item{Results.M4}{The results (powers and AIC values) of the fractional polynomials of order \eqn{4}.}
\item{Results.M5}{The results (powers and AIC values) of the fractional polynomials of order \eqn{5}.}
}
\references{
Van der Elst, W., Molenberghs, G., Hilgers, R., & Heussen, N. (2015). Correlation in continuous monitoring of vital parameters I - estimating reliability using linear mixed-effects models. \emph{Submitted.}
}
\author{
Wim Van der Elst, Geert Molenberghs, Ralf-Dieter Hilgers, & Nicole Heussen
}
\examples{
# Open data
data(Example.Data)

# Fit fractional polynomials, mox. order = 3
FP <- Fract.Poly(Covariate = Time, Outcome = Outcome, 
Dataset = Example.Data, Max.M=3)

# Explore results
summary(FP)
# best fitting model (based on AIC) for m=3, 
# powers:  p_{1}=3,  p_{2}=3, and  p_{3}=2

# Fit model and compare with observed means

    # plot of mean 
Spaghetti.Plot(Dataset = Example.Data, Outcome = Outcome,
Time = Time, Id=Id, Add.Profiles = FALSE, Lwd.Me=1, 
ylab="Mean Outcome")

    # Coding of predictors (note that when p_{1}=p_{2},
    # beta_{1}*X ** {p_{1}} + beta_{2}*X ** {p_{1}} * log(X)
    #  and when p=0, X ** {0}= log(X)    )
term1 <- Example.Data$Time**3
term2 <- (Example.Data$Time**3) * log(Example.Data$Time) 
term3 <- Example.Data$Time**2 

    # fit model
Model <- lm(Outcome~term1+term2+term3, data=Example.Data)
Model$coef # regression weights (beta's)  

    # make prediction for time 1 to 47
term1 <- (1:47)**3 
term2 <- ((1:47)**3) * log(1:47)   
term3 <- (1:47)**2 
    # compute predicted values
pred <- Model$coef[1] + (Model$coef[2] * term1) + 
  (Model$coef[3] * term2) + (Model$coef[4] * term3)
   # Add predicted values to plot
lines(x = 1:47, y=pred,  lty=2)
legend("topright", c("Observed", "Predicted"), lty=c(1, 2))
}
\keyword{Fractional polynomials}
