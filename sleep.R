source("funs.R")
## data <- read.csv("zeodata-jan14.csv")
## data <- read.csv("zeodata-march.csv")

data <- read.csv("zeodata-jun16.csv")
n <- nrow(data)

source("indices.R")

source("cleanup.R")


##########################
## Measurement Problems ##
##########################

### HOW DO WE MEASURE TIREDNESS?
##
## TIREDNESS should predict Time.in.Wake the day after (i.e. not wanting to get out of bed)

plotAndCorTest(data$nightLen, data$Total.Z)
plotAndCorTest(data$ZQ, data$Total.Z)

png(".png", width=750, height=300)
plot(data$ZQ*3, type="o", ylim=c(0,500))  ## Total.Z
abline(h=quantile(data$ZQ*3,c(.25,.5,.75)), col="grey")

pplot(data$Time.in.Wake, type="o", col="red") ## Time.in.Wake
abline(h=quantile(data$Time.in.Wake,c(.25, .5, .75)), col="#FF000044")

pplot(zqRestedness*3, type="o", col="#00FF00") ## Time.in.Wake
abline(h=quantile(data$Time.in.Wake,c(.5, .75)), col="#FF000044")

title("black: ZQ     red: Time.in.Wake")
dev.off()



##plotAndCorTest(data$Total.Z[1:(n-1)], data$Time.in.Wake[2:n])
##plotAndCorTest(data$totalTime[1:(n-1)], data$Time.in.Wake[2:n])

## ZQ seems to be the best predictor of Time.in.Wake the next day
plotAndCorTest(data$ZQ[1:(n-1)], data$Time.in.Wake[2:n])

plotAndCorTest(data$ZQ[1:n], data$Time.in.Wake[1:n])




plotAndCorTest(zqRestedness[1:(n-1)], data$Time.in.Wake[2:n])

kernelSd <- 0.5
zqRestedness <- gSmooth(data$dayNumber, data$ZQ, kernel=emaKernel)$y
timeRestedness <- gSmooth(data$dayNumber,data$Total.Z, kernel=emaKernel)$y

reg <- lm(data$Time.in.Wake[2:n] ~ data$ZQ[1:(n-1)])
summary(reg)


## kernelSd=5:    -0.522, -0.162
## kernelSd=3:    -0.554, -0.206
## kernelSd=1:    -0.581, -0.243
## kernelSd=0.5:  -0.562, -0.217



## zqRestedness, with a kernel of 1


#########################
## PLOT ALL NIGHTS     ##
#########################

source("funs.R")
plotNights(1:n)
plotNightsLeftAligned(1:n)


plotNights(90:n, startPlot=21)
plotNightsLeftAligned(1:n)


plotNights((n-30):n, fontSize=0.7)
plotNightsLeftAligned((n-30):n, fontSize=NA)


## Why do some nights have so much graph data?

sgl <- c()
for(i in 1:n)
  sgl[i] <- length(data$Sleep.Graph[[i]])

plot(data$Total.Z, sgl)

text(data$Total.Z[112], sgl[112], label="112")


aloneData <- data[50:134,]
mariaData <- data[135:n,]

t.test(mariaData$Start.of.Night.time,  aloneData$Start.of.Night.time)
t.test(mariaData$End.of.Night.time,  aloneData$End.of.Night.time)

t.test(mariaData$ZQ,  aloneData$ZQ)
ks.test(mariaData$Total.Z,  aloneData$Total.Z)

ks.test(mariaData$End.of.Night.time,  aloneData$End.of.Night.time)


#########################
## TEST HYPOTHESES     ##
#########################

## COMPENSATION: hypothesis: current night will be negatively correlated with recent nights.

## Conclusion: there is no evidence!

m <- arModels(data$Total.Z)
summary(m[[3]])


m <- arModelSum(data$ZQ)
summary(m)



## DAYS OF THE WEEK

mon <- subset(data, dayOfTheWeek=="M")
tue <- subset(data, dayOfTheWeek=="T")
wed <- subset(data, dayOfTheWeek=="W")
thu <- subset(data, dayOfTheWeek=="R")
fri <- subset(data, dayOfTheWeek=="F")
sat <- subset(data, dayOfTheWeek=="Sa")
sun <- subset(data, dayOfTheWeek=="Su")


l <- list(mon, tue, wed, thu, fri, sat, sun)

png("totalZ-violins.png", width=800, height=800)
violinPlot(l, Total.Z, col=c(rep("#AAAAFF",5), rep("orange", 2)), pointColors="white", bgColor="black", labelColor="white", xaxt="n", xlim=c(200,660))
##abline(v=c(5,6,7,8,9)*60,col="grey")
abline(v=quantile(data$Total.Z, c(0.25,.5,.75)),col="yellow", lty=2)
axis(1, at=c(5,6,7,8,9,10)*60, labels=c("5h","6h","7h","8h","9h","10h"))
axis(3, at=quantile(data$Total.Z, c(0.25,.5,.75)), labels=c("25%","50%","75%"))



violinPlot(l, ZQ, col=c(rep("#AAAAFF",5), rep("orange", 2)), pointColors="white", bgColor="black", labelColor="white", xaxt="n", xlim=c(40,130))
##abline(v=c(5,6,7,8,9)*60,col="grey")
abline(v=quantile(data$ZQ, c(0.25,.5,.75)),col="yellow", lty=2)
axis(3, at=quantile(data$ZQ, c(0.25,.5,.75)), labels=c("25%","50%","75%"))




violinPlot(l, Total.Z, col=c(rep("#AAAAFF",5), rep("orange", 2)), horizontal=FALSE, pointColors="white", bgColor="black")

violinPlot(l, ZQ, col=c(rep("#AAAAFF",5), rep("orange", 2)), pointColors="white", bgColor="black")
violinPlot(l, ZQ, col=c(rep("#AAAAFF",5), rep("orange", 2)), horizontal=FALSE, pointColors="white", bgColor="black")

HALF_VIOLIN <- TRUE
HALF_VIOLIN <- FALSE
violinPlot(l, ZQ, col=c(rep("#AAAAFF",5), rep("orange", 2)), horizontal=FALSE, pointColors="white", bgColor="black")
abline(h=quantile(data$ZQ,c(0.25, 0.5, 0.75)), col="grey")



l <- list(mon, tue, wed, thu, fri, sat, sun)
violinPlot(l, ZQ/Total.Z, col=c(rep("#AAAAFF",5), rep("orange", 2)), horizontal=FALSE)



violinPlot(l, Total.Z, col=c(rep("#AAAAFF",5), rep("orange", 2)), horizontal=FALSE)

violinPlot(l, "ZQ", col=c(rep("#AAAAFF",5), rep("orange", 2)), horizontal=FALSE)

violinPlot(l, ZQ/Total.Z, col=c(rep("#AAAAFF",5), rep("orange", 2)), horizontal=FALSE)


data$isSat <- data$dayOfTheWeek=="Sa"
data$isSun <- data$dayOfTheWeek=="Su"
data$isMon <- data$dayOfTheWeek=="M"
data$isTue <- data$dayOfTheWeek=="T"
data$isWed <- data$dayOfTheWeek=="W"
data$isThu <- data$dayOfTheWeek=="R"
data$isFri <- data$dayOfTheWeek=="F"

r <- lm(ZQ ~ isSun, data=data)
summary(r)

r <- lm(Time.in.REM+Time.in.Deep ~ isSun, data=data)
summary(r)



boxplot(Total.Z/60 ~ dayOfTheWeek, data=data)
abline(h=c(6,7,8), col="grey")


r1 <- lm(ZQ ~ 1, data=data)
r2 <- lm(ZQ ~ 1+dayOfTheWeek, data=data)
anova(r1,r2)

r2 <- lm(Start.of.Night.time ~ 1+dayOfTheWeek, data=data)
summary(r2)

r2 <- lm(End.of.Night.time ~ 1+dayOfTheWeek, data=data)
summary(r2)



plot(1:7, rnorm(7), main = "axis() examples",
          type = "s", xaxt = "n", frame = FALSE, col = "red")
     axis(1, 1:7, LETTERS[1:7], col.axis = "blue")
     # unusual options:
     axis(4, col = "violet", col.axis="dark violet", lwd = 2)
     axis(3, col = "gold", lty = 2, lwd = 0.5)


##
sun[3:16,]$Sleep.Date
fri[c(1:7,9:15),]$Sleep.Date


sun[c(4,6:16),]$Sleep.Date
sat[2:13,]$Sleep.Date
fri[c(2,4:7,9:15),]$Sleep.Date

suns <- sun[c(4,6:16),]
sats <- sat[2:13,]
fris <- fri[c(2,4:7,9:15),]

r <- lm(suns$ZQ ~ sats$ZQ)
summary(r)
plot(suns[c(1,2,4:12),]$ZQ, sats[c(1,2,4:12),]$ZQ, labels=1:nrow(suns))
text(suns[c(1,2,4:12),]$ZQ, sats[c(1,2,4:12),]$ZQ, labels=1:nrow(suns))
plotAndCorTest(suns[c(1,2,4:12),]$ZQ, sats[c(1,2,4:12),]$ZQ)

cor.test(suns$ZQ, sats$ZQ)


## AUTOCORRELATIONS

plotAndCorTest(data$Total.Z[1:(n-1)], data$Total.Z[2:n])  ## Total time, Nope

plotAndCorTest(data$ZQ[1:(n-1)], data$ZQ[2:n])  ## ZQ, maybe...

plotAndCorTest(data$Time.in.Deep[1:(n-1)], data$Time.in.Deep[2:n]) ## Deep sleep, yes!
cor.test(data$Time.in.Deep[1:(n-1)], data$Time.in.Deep[2:n])

plotAndCorTest(data$Time.in.REM[1:(n-1)], data$Time.in.REM[2:n]) ## REM sleep, YES!
cor.test(data$Time.in.REM[1:(n-1)], data$Time.in.REM[2:n])


plotAndCorTest(data$Total.Z[1:n], data$Time.in.Wake[1:n], showFit="lowess")
plotAndCorTest(data$Total.Z[1:(n-1)], data$Time.in.Wake[2:n])




x <- data$Total.Z[1:n]
y <- data$Time.in.Wake[1:n]


par(mfrow=c(1,3))
plotAndCorTest(data$Start.of.Night.time, data$Total.Z, showFit="linear", xaxt="n", yaxt="n"); axis(1, 60*(1:6), jPaste(1:6,"am")); abline(v=60*c(0:5), col="grey")
axis(2, 60*(5:9), jPaste(5:9,"h"))
abline(h=60*(5:9), col="grey")

plotAndCorTest(data$End.of.Night.time, data$Total.Z, showFit="linear", xlim=60*c(6,13), xaxt="n", yaxt="n")
axis(1, 60*(5:13), c(jPaste(5:11,"am"), "noon", "1pm"))
axis(2, 60*(5:9), jPaste(5:9,"h"))
abline(v=60*c(5:13), col="grey")
abline(h=60*c(5:9), col="grey")



plotAndCorTest(data$Start.of.Night.time, data$End.of.Night.time, showFit="linear", xaxt="n", yaxt="n", ylim=60*c(7,13))
axis(1, 60*(1:6), jPaste(1:6,"am")); abline(v=60*c(0:5), col="grey")
axis(2, 60*(6:13), c(jPaste(6:11,"am"),"noon", "1pm"))
abline(h=60*c(5:13), col="grey")

text(60*1, 60*13, "TotalZ large")
text(60*5, 60*7, "TotalZ small")




cor.test(data$End.of.Night.time, data$Total.Z, method="spearman")


plotAndCorTest(data$ZQ[1:(n-1)], data$Time.in.Wake[2:n], col="#00000044")


summary(lm(ZQ ~ Time.in.Deep + Time.in.REM + Time.in.Light, data=data))



par(mfrow=c(2,2))

png("total-series.png", width=750, height=300)
plot(data$dayNumber, data$Total.Z, type="o")
abline(h=quantile(data$Total.Z, c(0.25,.5,.75)), col="grey")
dev.off()


plot(data$dayNumber, data$Time.in.Deep, type="o")
abline(h=quantile(data$Time.in.Deep, c(0.25,.5,.75)), col="grey")


plot(data$dayNumber, data$Time.in.Light, type="o")
abline(h=quantile(data$Time.in.Light, c(0.25,.5,.75)), col="grey")


png("REM-autocorrelations.png", width=750, height=300)
plot(data$dayNumber, data$Time.in.REM, type="o")
abline(h=quantile(data$Time.in.REM, c(0.25,.5,.75)), col="grey")
dev.off()


par(mfrow=c(1,1))
plot(data$dayNumber, data$deepProp, type="o", ylim=c(0,1))
pplot(data$dayNumber, data$remProp, type="o", col="blue")
pplot(data$dayNumber, data$lightProp, type="o", col="green")


plot(0,0,type="n", xlim=range(data$dayNumber), ylim=c(0,600), ylab="Time slept in each phase", xlab="day", xaxt="n"); axis(1, c(4384,4415,4444,4475), c("Jan 1", "Feb 1","Mar 1","Apr 1"))
jPolygon(data$dayNumber, data$Total.Z, col="grey")
jPolygon(data$dayNumber, data$Time.in.Deep+data$Time.in.REM, col="blue")
jPolygon(data$dayNumber, data$Time.in.Deep, col="black")
abline(v=sun$dayNumber, col="orange", lty=2)
abline(h=quantile(data$Total.Z,c(.25,.5,.75)),col="#AAAAAA", lty=2)
abline(h=quantile(data$Time.in.REM+data$Time.in.Deep,c(.25,.5,.75)),col="#6666BB", lty=2)
pplot(data$dayNumber, data$Time.in.Wake, col="red", type="l")

legend("topleft", legend = c("light", "REM", "deep", "awake in bed"), col=c("grey","blue", "black", "red"), lwd=7, lty=1, merge=TRUE)





plot(data$dayNumber, data$remProp, type="o", ylim=c(0,0.4))
abline(h=quantile(data$remProp, c(0.25,.5,.75)), col="grey")
pplot(data$dayNumber, data$Time.in.Wake/500, type="o", col="red")

plot(data$dayNumber, data$lightProp, type="o", ylim=c(0,0.7))
abline(h=quantile(data$lightProp, c(0.25,.5,.75)), col="grey")
pplot(data$dayNumber, data$Time.in.Wake/500, type="o", col="red")





plotAndCorTest(data$Time.in.Light[1:(n-1)], data$Time.in.Light[2:n]) ## light sleep, no


plot(data$dayNumber, data$Time.in.REM, type="o")

## Hypothesis: REM sleep is associated with tiredness in the previous day
plotAndCorTest(zqRestedness[1:(n-1)], data$Time.in.REM[2:n])

plotAndCorTest(zqRestedness[1:(n-1)], data$Total.Z[2:n])


cor.test(data$Time.in.REM[2:n], zqRestedness[1:(n-1)])

plotAndCorTest(data$Time.in.Deep[2:n], timeRestedness[1:(n-1)])



cor.test(data$Time.in.Deep[1:(n-1)], data$Time.in.Deep[2:n])





r <- mean(data$ZQ)/mean(data$Total.Z)
plot(data$dayNumber,data$ZQ, type="l")
pplot(data$dayNumber,r*data$Total.Z, type="l", col="red")
title("black: ZQ    red: Total.Z")


## Hypothesis: when well-rested in *previous* nights, I sleep less efficiently

par(mfrow=c(2,1))

plot(data$dayNumber, data$Total.Z, type="o")
pplot(data$dayNumber, timeRestedness, type="l", col="red")
abline(h=quantile(timeRestedness, c(.25,.5,.75)), col="#FF000044")

plot(data$dayNumber, data$ZQ, type="o")
pplot(data$dayNumber, zqRestedness, type="l", col="red")
abline(h=quantile(zqRestedness, c(.25,.5,.75)), col="#FF000044")



plotAndCorTest(timeRestedness[1:(n-1)], data$Total.Z[2:n])
plotAndCorTest(timeRestedness[1:(n-1)], data$ZQ[2:n])



ct <- cor.test(data$Total.Z,data$ZQ)
plot(data$nightLengthInMin,data$ZQ)
text(data$nightLengthInMin,data$ZQ,labels=1:n)

cor.test(data$Total.Z,data$ZQ)
plot(data$Total.Z,data$ZQ)
text(data$Total.Z,data$ZQ,labels=1:n)



## ZQ
##1-day auto-correlation
##2-day auto-correlation

plot(data$ZQ, type="l")

plot(data$Start.of.Night.time, type="l")

s <- data$Total.Z[1:n]
n <- length(s)
  
k <- 2
s1 <- s[1:(n-k)]
s2 <- s[(k+1):n]
plot(s1, s2)
cor.test(s1, s2)







plot(data$Start.of.Night.time, data$End.of.Night.time, type="p", asp=1, xlim=c(0,900), ylim=c(0,900), col="#00000088")
text(data$Start.of.Night.time, data$End.of.Night.time, labels=1:n)
cor.test(data$Start.of.Night.time, data$End.of.Night.time)


## remove outliers

outliers <- c(35,42)
newData <- data[-outliers,]
plot(newData$Start.of.Night.time, newData$End.of.Night.time, type="p", asp=1, xlim=c(0,500),
     ylim=c(400,900), col="#00000088")

cor.test(data$Start.of.Night.time, data$End.of.Night.time)



plot(newData$Start.of.Night.time, newData$ZQ, type="p", col="#00000044")
cor.test(newData$Start.of.Night.time, newData$ZQ)

plot(newData$End.of.Night.time, newData$ZQ, type="p", col="#00000044")
cor.test(newData$End.of.Night.time, newData$ZQ)

## regress ZQ on 


## create a "sleep debt" index, i.e. exponential moving average of recent debt.


## the longest streak of ZQ > 90





plot(density(data$ZQ))
pplot(data$ZQ, 0.001*runif(n), col="#00000066")
abline(v=10*(1:11), col="grey")


plot(density(data$ZQ))
pplot(data$ZQ, 0.001*runif(n), col="#00000066")
abline(v=10*(1:11), col="grey")

plot(density(data$Total.Z/60))
abline(v=(1:11), col="grey")
pplot(data$Total.Z/60, 0.01*runif(n), col="#00000066")





## H_0: the 'Total.Z' data are IID
## H_A: we compensate for short nights with long nights.
## smoothing statistic: the variance of the smoothed 'Total.Z' data
## what is the distribution of the statistic when these data are IID?


plot(data$dayNumber, data$ZQ, type="o")
kernelSd <- 3
ss <- gSmooth(data$dayNumber,data$ZQ)
pplot(ss$x,ss$y, type="l", col="red")
kernelSd <- 3
ss <- gSmooth(data$dayNumber,data$ZQ, kernel=emaKernel)
pplot(ss$x,ss$y, type="l", col="blue")



## shifting variance model: some time periods are allowed to have more variance than others.
##



#####################
## AUTOREGRESSIONS ##
#####################

## ToDo: code to produce lagged series, but only when the difference between the days is exactly 'k'


s <- data$Total.Z

r1 <- s[1:(n-2)]
r2 <- s[2:(n-1)]
r3 <- s[3:n]

reg <- lm(r3~r1+r2)
summary(reg)


s1 <- s[1:(n-3)]
s2 <- s[2:(n-2)]
s3 <- s[3:(n-1)]
s4 <- s[4:n]

reg <- lm(s4~s1+s2+s3)
summary(reg)


anova(lm(s1~rep(0,length(s1))), lm(s1~s2+s3+s4))



## (In the last line here, it is not apparent that there is any dependence on 'x', but when you look at 'withExpr', you see that 'x' will come out after the evaluation;  a language with a strict static-checker would have given me an annoying warning/error.)
ife
