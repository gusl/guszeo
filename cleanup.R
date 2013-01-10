## CLEAN UP
data$Start.of.Night <- sapply(1:n , function(i) toString(data[i,]$Start.of.Night))
data$Start.of.Night.time <- sapply(data$Start.of.Night, function(x) strsplit(x," ")[[1]][2])
data$Start.of.Night.time <- sapply(data$Start.of.Night.time, convertToMinutes)

data$End.of.Night <- sapply(1:n , function(i) toString(data[i,]$End.of.Night))
data$End.of.Night.time <- sapply(data$End.of.Night, function(x) strsplit(x," ")[[1]][2])
data$End.of.Night.time <- sapply(data$End.of.Night.time, convertToMinutes)

data$Rise.Time <- sapply(1:n , function(i) toString(data[i,]$Rise.Time))
data$Rise.Time <- sapply(data$Rise.Time, function(x) strsplit(x," ")[[1]][2])
data$Rise.Time <- sapply(data$Rise.Time, convertToMinutes)

data$Sleep.Graph <- sapply(1:n , function(i) strsplit(toString(data[i,]$Sleep.Graph), " ")[[1]])
data$Sleep.Graph <- sapply(1:n , function(i) as.numeric(data$Sleep.Graph[[i]]))


data$Detailed.Sleep.Graph <- sapply(1:n , function(i) as.numeric(strsplit(toString(data[i,]$Detailed.Sleep.Graph), " ")[[1]]))

data$Sleep.Date <- sapply(data$Sleep.Date, toString)



## graph3
## This results in mostly the removal of trailing zeros from Sleep.Graph.
for (i in 1:n){
  start <- data$Start.of.Night.time[i]
  end <- data$End.of.Night.time[i]
  if (start>end) start <- start-1440
  
  timeGrid1 <- seq(start, end, by=5)
  graph1 <- data[i,]$Sleep.Graph[[1]]
  data$graph3[[i]] <- graph1[1:length(timeGrid1)]
  ##print warning
  inspect(i)
  print(jPaste("Throwing out ", length(graph1)-length(data$graph3[[i]]), " entries"))
  thrownOut <- graph1[jSeq(length(timeGrid1)+1,length(graph1))]
  inspect(thrownOut)
}





###############################
## Computing new quantities ###
###############################

data$totalTime <- data$Time.in.Wake + data$Time.in.REM + data$Time.in.Light + data$Time.in.Deep
data$nightLengthInMin <- data$End.of.Night.time - data$Start.of.Night.time
data$lengthOfSleepGraph <- sapply(1:n, function(i) length(data$Sleep.Graph[[i]]))

data$wakeProp <- data$Time.in.Wake/data$totalTime
data$remProp <- data$Time.in.REM/data$totalTime
data$lightProp <- data$Time.in.Light/data$totalTime
data$deepProp <- data$Time.in.Deep/data$totalTime
data$sleepEfficiency <- data$ZQ/data$totalTime

data$dayNumber <- sapply(data$Sleep.Date, getDayNumber)

data$dayOfTheWeek <- sapply(data$Sleep.Date, getDayOfWeek)

