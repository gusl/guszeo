convertToMinutes <- function(timeString){
  split <- strsplit(timeString, ":")[[1]]
  60*as.numeric(split[1])+as.numeric(split[2])
}

convertToHM <- function(minutes){
  h <- floor(minutes/60)
  m <- minutes - 60*h
  if (m<10)
    m <- jPaste("0", m)
  jPaste(h,":",m)
}


## returns a number between 1 and 366
computeDayOfYear <- function(day, month, isLeapYear){
  monthLengths <- c(31,ifelse(isLeapYear,29,28),31,30,31,30,31,31,30,31,30,31)
  total <- day  
  if (month>1) total <- total+ sum(monthLengths[1:(month-1)])
  total
}


##computeDayOfYear(1,25, isLeapYear=FALSE)
##computeDayOfYear(2,11, isLeapYear=FALSE)
##computeDayOfYear(12,31, isLeapYear=FALSE)

getDayOfYear <- function(dateString){
  sp <- strsplit(dateString, split="/")[[1]]
  month <- as.numeric(sp[1])
  day <- as.numeric(sp[2])
  year <- as.numeric(sp[3])
  computeDayOfYear(day, month, isLeapYear=isLeap(year))
}

## day index, counting from 1 Jan 2000
getDayNumber <- function(dateString){
  sp <- strsplit(dateString, split="/")[[1]]
  month <- as.numeric(sp[1])
  day <- as.numeric(sp[2])
  year <- as.numeric(sp[3])
  doy <- computeDayOfYear(day, month, isLeapYear=isLeap(year))

  years <- 2000:2039
  yearIndices <- jSeq(2000,year-1)-1999
  yearLengths <- rep(c(366,365,365,365),10)
  yearDays <- sum(yearLengths[yearIndices])
  
  yearDays+doy
}




getDayOfWeek2012 <- function(dateString){
  daysOfWeek <- c("Sa", "Su", "M", "T", "W", "R", "F")
  daysOfWeek[getDayOfYear(dateString)%%7+1]
}

getYearPattern <- function(year){
  if(year==2012) return(c("Sa", "Su", "M", "T", "W", "R", "F"))
  if(year==2011) return(c("F", "Sa", "Su", "M", "T", "W", "R"))
}

isLeap <- function(year) year%%4==0

getDayOfWeek <- function(dateString){
  sp <- strsplit(dateString, split="/")[[1]]
  year <- as.numeric(sp[3])
  daysOfWeek <- getYearPattern(year)
  daysOfWeek[getDayOfYear(dateString)%%7+1]
}


getDayOfWeek2012("02/11/2012")





## convert to minutes: hours*60+minutes.  If end < start, then you woke up on a different day.
makeTimeGrid <- function(startTime, endTime) seq(startTime,endTime, by=5)

removeTrailingZeros <- function(v){
  flag <- FALSE ## does something get removed?
  for (end in length(v):1){
    if (v[end]!=0)
      break
    flag <- TRUE ## something gets removed
  }
  v[1:end]
  ##ifelse (flag, c(v[1:end],0), v[1:end])
}

removeLeadingZeros <- function(v){
  for (start in 1:length(v)){
    if (v[start]!=0)
      break
  }
  v[start:length(v)]
}


## doPlot <- function(x, y, start, ...){
##   grid <- seq(0,12*60,by=5)/60
  
##   nLeadingNulls <- start/5  ## remove this
##   nTrailingNulls <- 12*12+1 - length(y) - nLeadingNulls ##  why -2?
##   inspect(y)
##   inspect(nLeadingNulls)
##   inspect(nTrailingNulls)
##   leadingNulls <- rep(NA,nLeadingNulls)
##   trailingNulls <- rep(NA,nTrailingNulls)
  
##   graph <- c(leadingNulls, y, trailingNulls)

##   inspect(length(grid))
##   inspect(length(graph))

##   plot(grid,graph, xaxp=c(0,12,12), ...)
## }


doPlot <- function(x, y, ...){
  plot(x,y, xaxp=c(0,12,12), ...)
}


plotNight <- function(index){
  start <- data$Start.of.Night.time[index]
  end <- data$End.of.Night.time[index]
  
  timeGrid1 <- seq(start, end, by=5)
  graph1 <- data[index,]$Sleep.Graph[[1]]  
  graph3 <- graph1[1:length(timeGrid1)]

  doPlot(timeGrid1, -graph3, col="green", type="l", xlab=jPaste(convertToHM(start), " to ", convertToHM(end), " = ", convertToHM(end - start), " total"), ylab="Sleep stage", xlim=c(-30,12*120), ylim=c(-5,0))

  abline(v=c(start,data$Rise.Time[index]))

  title(data$Sleep.Date[index], jPaste("ZQ = ", data$ZQ[index]))
}


## start grid from 9pm

## Multiple nights on a single plot, as a column

## startPlot is the time in which the plot begins (from 0 to 23)... 21=9pm
plotNights <- function(indices, fontSize=NA, startPlot=21, ...){
  offset <- 60*(24-startPlot)

  plot(0,0,type="n",xlim=c(0,12*120), ylim=c(-5,5*length(indices)), xaxt="n", yaxt="n", xlab="", ylab="", ...)
  abline(h=0)
  abline(v=60*c(0,3,6,9,12,15), col="grey")
  text(60*c(0,3,6,9,12,15),c(0,0,0,0,0,0)-10, labels=c("9pm", "midnight","3am","6am", "9am", "noon"))
  text(c(1100,1200,1300),c(0,0,1)-10, labels=c("time slept", "ZQ", "date \n(before midnight)"), cex=0.6)
  yNotches <- 10*(1:11)
  text(rep(0,length(yNotches))-30, 5*yNotches-2.5, labels=yNotches, cex=0.5)

  ##abline(h=5*69, col="purple") ## DST daylight savings

  if (is.na(fontSize)) fontSize <- min(45/length(indices), 1)
  prevDoy <- -1

  ## 
  for (i in seq_len(length(indices))){
    index <- indices[i]
    inspect(index)
    start <- data$Start.of.Night.time[index] + offset
    end <- data$End.of.Night.time[index] + offset
    if (start>end) start <- start-1440
    inspect(start)
    inspect(end)

    timeGrid1 <- seq(start, end, by=5)

    graph3 <- data$graph3[[index]]
    
    rect(start,5*i,data$Rise.Time[index]+offset,5*(i-1), border=NA, col="#00000022")
    print("h1")
    inspect(timeGrid1)
    inspect(graph3)
    inspect(length(timeGrid1))
    inspect(length(graph3))

    pplot(timeGrid1, -graph3+i*5, col="#00AA00", type="l")
    abline(h=5*i, col="grey")
    points(c(start, start), c(5*i,5*(i-1)), type="l", col="grey")
    points(c(data$Rise.Time[index]+offset,data$Rise.Time[index]+offset), c(5*i,5*(i-1)), type="l", col="grey")
    print("h5")

    doy <- getDayOfYear(toString(data[index,]$Sleep.Date))
    ##points(c(1000+3*doy,1000+3*doy),c(5*i,5*(i-1)), type="l", col="grey")
    rect(1000+3*doy,5*(i-1),1010+3*doy,5*i, border=NA, col="grey")

    if (abs(doy-prevDoy)!=1)
          abline(h=5*(i-1), col="black")

    prevDoy <- doy    
    
    dayOfTheWeek <- data[index,]$dayOfTheWeek ##getDayOfWeek(toString(data[index,]$Sleep.Date))
    
    
    color <- ifelse(dayOfTheWeek=="Sa" ||dayOfTheWeek=="Su", "orange3", "blue")
    text(0, 5*i-2.5, dayOfTheWeek, col=color, cex=fontSize)  ##MTWRFSaSu
    text(1300, 5*i-2.5, toString(data[index,]$Sleep.Date), col=color, cex=fontSize)
    text(1200, 5*i-2.5, data$ZQ[index], col=color, cex=fontSize)
    text(1100, 5*i-2.5, convertToHM(data[i,]$Total.Z), col=color, cex=fontSize)
  }
}

plotNightsLeftAligned <- function(indices, fontSize=NA){
  
  plot(0,0,type="n",xlim=c(-100,12*120), ylim=c(-5,5*length(indices)), xaxt="n", yaxt="n", xlab="", ylab="")
  abline(h=0)
  abline(v=60*c(0,3,6,7,8,9,10,12), col="grey")
  abline(v=60*8, col="#00000044")

  text(c(1100,1200,1300),c(0,0,1)-10, labels=c("time slept", "ZQ", "date \n(before midnight)"), cex=0.6)
  yNotches <- c(10,20,30,40,50,60,70,80)
  text(rep(0,length(yNotches))-135, 5*yNotches-2.5, labels=yNotches, cex=0.5)
  
  if (is.na(fontSize)) fontSize <- min(45/length(indices), 1)
  prevDoy <- -1
    
  for (i in seq_len(length(indices))){
     index <- indices[i]
     start <- data$Start.of.Night.time[index]
     end <- data$End.of.Night.time[index]
     timeGrid1 <- seq(start, end, by=5)

    ##rect(0,5*i,data$Rise.Time[index]-start,5*(i-1), border=NA, col="#00000022")

    graph3 <- data$graph3[[index]]
      
    pplot(timeGrid1-start, -graph3+i*5, col="#00AA00", type="l")    
    abline(h=5*i, col="grey")
    points(c(0, 0), c(5*i,5*(i-1)), type="l", col="grey")
    points(c(data$Rise.Time[index] - start,data$Rise.Time[index] - start), c(5*i,5*(i-1)), type="l", col="grey")


    text(-50, 5*i-2.5, convertToHM(start), cex=fontSize)
    text(end-start+50, 5*i-2.5, convertToHM(end), cex=fontSize)
    

    doy <- getDayOfYear(toString(data[index,]$Sleep.Date))
    ##points(c(1000+3*doy,1000+3*doy),c(5*i,5*(i-1)), type="l", col="grey")
    rect(1000+3*doy,5*(i-1),1010+3*doy,5*i, border=NA, col="grey")

    w <- data[index,]$Time.in.Wake
    r <- data[index,]$Time.in.REM
    l <- data[index,]$Time.in.Light
    d <- data[index,]$Time.in.Deep
    
    ##rect(0,5*(i-1),0+3*data[i,]$ZQ,5*i, border=NA, col="#FF000055")
    rect(0, 5*(i-1), d, 5*i, border=NA, col="#00880055")
    rect(d, 5*(i-1), d+r, 5*i, border=NA, col="#00FF0055")
    rect(d+r, 5*(i-1),d+r+l,5*i, border=NA, col="#AAAAAA55")
    rect(d+r+l, 5*(i-1), d+r+l+w,5*i, border=NA, col="#FF330055")

    ##if(data[i,]$ZQ>60) rect(770,5*(i-1),800,5*i, border=NA, col="#000000AA")
    ##if(data[i,]$ZQ>70) rect(800,5*(i-1),830,5*i, border=NA, col="#000000AA")
    ##if(data[i,]$ZQ>80) rect(830,5*(i-1),860,5*i, border=NA, col="#000000AA")
    ##if(data[i,]$ZQ>90) rect(860,5*(i-1),890,5*i, border=NA, col="#000000AA")
    ##if(data[i,]$ZQ>100) rect(890,5*(i-1),920,5*i, border=NA, col="#000000AA")

    ## DST started Mar 11
    ## 70th entry is the night after Mar 11, which is the first night with the new clock setting
    ## add a line between 69 and 70

    
    if (abs(doy-prevDoy)!=1)
          abline(h=5*(i-1), col="black")

    prevDoy <- doy
    
    dayOfTheWeek <- getDayOfWeek(toString(data[index,]$Sleep.Date))
    color <- ifelse(dayOfTheWeek=="Sa" ||dayOfTheWeek=="Su", "orange3", "blue")
    text(-110, 5*i-2.5, dayOfTheWeek, col=color, cex=fontSize)  ##MTWRFSaSu
    text(1300, 5*i-2.5, toString(data[index,]$Sleep.Date), col=color, cex=fontSize)
    text(1200, 5*i-2.5, data$ZQ[index], col=color, cex=fontSize)
    text(1100, 5*i-2.5, convertToHM(data[i,]$Total.Z), col=color, cex=fontSize)
  }
}


gSmooth <- function(x,y, kernel=gaussKernel){
  v <- c()
  for (i in seq_len(length(x))){
    center <- x[i]
    weights <- sapply(x, function(z) kernel(z, center))
    v[i] <- sum(weights*y)/sum(weights)
  }
  list(x=x,y=v)
}

##function(z) if(z<=center) return(exp(kernelSd*(z-center))) else return(0)

gaussKernel <- function(z, center) dnorm(z, mean=center, sd=kernelSd)
emaKernel <- function(z, center) {
  v <- c()
  v[z>center] <- 0
  v[z<=center] <- exp((z-center)/kernelSd)
  v
}


makePrettyViolinPlot <- function(l, property){
  violinPlot(l, property, col=c(rep("#AAAAFF",5), rep("orange", 2)), pointColors="white", bgColor="black", labelColor="white", xaxt="n", xlim=c(200,660))
  ##abline(v=c(5,6,7,8,9)*60,col="grey")
  axis(1, at=c(5,6,7,8,9,10)*60, labels=c("5h","6h","7h","8h","9h","10h"))
  axis(3, at=quantile(data$Total.Z, c(0.25,.5,.75)), labels=c("25%","50%","75%"))
  abline(v=quantile(data$Total.Z, c(0.25,.5,.75)),col="yellow", lty=2)

  
}
