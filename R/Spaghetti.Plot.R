Spaghetti.Plot <- function(Dataset, Outcome, Time, Id, Add.Profiles=TRUE,Add.Mean=TRUE, 
                           Add.Median=FALSE, Col=8, Lwd.Me=3, ...){

Object <- x <- Dataset   
Outcome <- x[,paste(substitute(Outcome))]
Time <- x[,paste(substitute(Time))] 
Id <- x[,paste(substitute(Id))]
Data <- data.frame(cbind(Outcome, Time, Id))

max_val_y <- max(Data$Outcome)
max_val_x <- max(Data$Time)
plot(y=Outcome, x=Time, type = "n", ylim=c(0, max_val_y), xlim=c(0, max_val_x), ...)

if (Add.Profiles==TRUE){
for (i in 1: length(unique(Id))){
  lines(y=Data$Outcome[Data$Id==unique(Data$Id)[i]], 
        x=1:length(Data$Outcome[Data$Id==unique(Data$Id)[i]]), col=Col)
 }
}

if (Add.Mean==TRUE){
mean <- 
  tapply(Data$Outcome, INDEX = Data$Time, FUN = mean)
lines(mean, lwd=Lwd.Me)
}

if (Add.Median==TRUE){
  median <- 
    tapply(Data$Outcome, INDEX = Data$Time, FUN = median)
  lines(median, lwd=Lwd.Me)
}


}