
lSE <- c()
lG <- c()
lG_removeTrailingZeros <- c()
lG_removeLeadingAndTrailingZeros <- c()
for (i in 1:n){
  inspect(i)
  lSE[i] <- length(seq(data$Start.of.Night.time[i], data$End.of.Night.time[i], by=5))
  lG[i] <- length(data$Sleep.Graph[[i]])
  lG_removeTrailingZeros[i] <- length(removeTrailingZeros(data$Sleep.Graph[[i]]))
  lG_removeLeadingAndTrailingZeros[i] <- length(removeLeadingZeros(removeTrailingZeros(data$Sleep.Graph[[i]])))
}

plot(lSE,lG, xlim=c(0,150), ylim=c(0,150), asp=1)
pplot(c(0,150), c(0,150), type="l", lty=2)

plot(lSE,lG_removeTrailingZeros, xlim=c(0,150), ylim=c(0,150), asp=1)
pplot(c(0,150), c(0,150), type="l", lty=2)

zeros <- which(lG_removeTrailingZeros - lSE == 0)
minus1s <- which(lG_removeTrailingZeros - lSE == -1)

## the ones that match exactly (zeros) are the ones where Sleep.Graph has trailing zeros
## the ones that are off by one (minus1s) are the ones where Sleep.Graph has no trailing zeros
## 
## Solution (hack): when removing trailing zeros, leave the last one.


plot(lSE,lG_removeLeadingAndTrailingZeros, xlim=c(0,150), ylim=c(0,150), asp=1)
pplot(c(0,150), c(0,150), type="l", lty=2)


lG_removeLeadingAndTrailingZeros - lSE



plot(data[1,]$Detailed.Sleep.Graph[1:10][[1]], type="l")
pplot(10*(1:length(data[1,]$Sleep.Graph[[1]])), data[1,]$Sleep.Graph[[1]], type="l", col="red")






## Start.of.Night = 04:00
## Start.of.Night.time = 240
##
## Q: why do we remove leading & trailing nulls?
## A: the Sleep.Graph vector typically begins with 0, 1 or 2 zeros, and ends with several 0s.
## 
## All we need to know is the time corresponding to each entry.
## 
## Approach #1: timeGrid <- seq(start, end, by=5)  ## define the time-grid as Start.of.Night to End.of.Night
## PROBLEM: the length of 'timeGrid' sometimes does not correspond to the length of SleepGraph. However, if
## we remove the trailing zeros from SleepGraph.
## 


