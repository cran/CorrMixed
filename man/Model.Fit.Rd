\name{Model.Fit}
\alias{Model.Fit}
\title{
Compare the fit of linear mixed-effects models
}
\description{
This function compares the fit of Model 1 (random intercept) and 2 (random intercept and Gausssian serial correlation), and of Model 2 (random intercept and Gausssian serial correlation) and 3 (random intercept, slope and Gausssian serial correlation)
}
\usage{
Model.Fit(Model.1, Model.2)
}
\arguments{
  \item{Model.1}{An object of class \code{WS.Corr.Mixed}, the first fitted model.}
  \item{Model.2}{Another object of class \code{WS.Corr.Mixed}, the second fitted model.}
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
data(Example.Data)

# Code predictors for time
Example.Data$Time2 <- Example.Data$Time**2
Example.Data$Time3 <- Example.Data$Time**3
Example.Data$Time3_log <- (Example.Data$Time**3) * (log(Example.Data$Time))

# model 1
Model1 <- WS.Corr.Mixed(
Fixed.Part=Outcome ~ Time2 + Time3 + Time3_log + as.factor(Cycle) 
+ as.factor(Condition), Random.Part = ~ 1|Id, 
Dataset=Example.Data, Model=1, Id="Id", 
Number.Bootstrap = 0, Seed = 12345)

# model 2
Model2 <- WS.Corr.Mixed(
Fixed.Part=Outcome ~ Time2 + Time3 + Time3_log + as.factor(Cycle) 
+ as.factor(Condition), Random.Part = ~ 1|Id, 
Correlation=corGaus(form= ~ Time, nugget = TRUE),
Dataset=Example.Data, Model=2, Id="Id", 
Number.Bootstrap = 0, Seed = 12345)

# model 3
Model3 <- WS.Corr.Mixed(
  Fixed.Part=Outcome ~ Time2 + Time3 + Time3_log + as.factor(Cycle) 
  + as.factor(Condition), Random.Part = ~ 1 + Time|Id, 
  Correlation=corGaus(form= ~ Time, nugget = TRUE),
  Dataset=Example.Data, Model=3, Id="Id", 
  Number.Bootstrap = 0, Seed = 12345)

# compare models 1 and 2
Model.Fit(Model.1=Model1, Model.2=Model2)

# compare models 2 and 3
Model.Fit(Model.1=Model2, Model.2=Model3)
}
\keyword{Compare model fit}
\keyword{G2}
\keyword{Likelihood ratio}