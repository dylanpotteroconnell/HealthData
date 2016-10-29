# Dylan O'Connell

stepCount <- read.csv("stepCount.csv", as.is=TRUE)
distWalked <- read.csv("distWalked.csv", as.is=TRUE)
flightsClimbed <- read.csv("flightsClimbed.csv", as.is=TRUE)

stepCount$midtime <- as.POSIXct(stepCount$midtime, format="%Y-%m-%d %H:%M:%S", tz="EST")
distWalked$midtime <- as.POSIXct(distWalked$midtime, format="%Y-%m-%d %H:%M:%S", tz="EST")

#months(), weekdays()
start.date <- as.POSIXct("2016-09-01")
end.date <- as.POSIXct("2016-09-07")
#x <- distWalked[distWalked$midtime > start.date & distWalked$midtime < end.date ,]
x <- distWalked[months(distWalked$midtime) == "September",]
#x <- distWalked[as.Date(distWalked$midtime) == as.Date("2016-09-07"),]

# every 5 minutes from the start until the end of our dataset
time.interval = 900
timeseq <- seq(from=range(x$midtime)[1], to=range(x$midtime)[2], by=time.interval)
rollavg.45m <- rep(0, length(timeseq))
time.start <- as.numeric(range(x$midtime)[1])

# This as.integer will round some mid times up a half second, but this is
# all approximate
x$secFromStart <- as.integer(x$midtime) - time.start

for(i in 1:length(x$secFromStart)){
  rollavg.45m[x$secFromStart[i] / time.interval ] <- rollavg.45m[x$secFromStart[i] / time.interval] + x$value[i]
  rollavg.45m[x$secFromStart[i] / time.interval + 1] <- rollavg.45m[x$secFromStart[i] / time.interval + 1] + x$value[i]
  rollavg.45m[x$secFromStart[i] / time.interval + 2] <- rollavg.45m[x$secFromStart[i] / time.interval + 2] + x$value[i]
}

rollavg.45m <- rollavg.45m[1:length(timeseq)]
plot(timeseq,rollavg.45m,pch=16,cex=0.55, 
     xlab="Time", ylab="Miles Travelled", 
     main="Rolling 45 minute average of distance travelled")


# To only plot the monday points, we strip the dates from midtime,
# and add identical days.
plotavgbyDOW = function(x, dow){
  x <- x[weekdays(x$midtime) == dow,]
  x$midtime <- strftime(x$midtime, format="%H:%M:%S")
  x$midtime <- as.POSIXct(x$midtime, format="%H:%M:%S")
  time.interval = 1800
  time.start <- as.numeric(range(x$midtime)[1])
  x$secFromStart <- as.integer(x$midtime) - time.start
  timeseq <- seq(from=range(x$midtime)[1], to=range(x$midtime)[2], by=time.interval)
  rollavg <- rep(0, length(timeseq))
  
  for(i in 1:length(x$secFromStart)){
    rollavg[x$secFromStart[i] / time.interval ] <- rollavg[x$secFromStart[i] / time.interval] + x$value[i]
    rollavg[x$secFromStart[i] / time.interval + 1] <- rollavg[x$secFromStart[i] / time.interval + 1] + x$value[i]
    rollavg[x$secFromStart[i] / time.interval + 2] <- rollavg[x$secFromStart[i] / time.interval + 2] + x$value[i]
  }
  
  rollavg <- rollavg[1:length(timeseq)]
  plot(timeseq,rollavg,pch=16,cex=.8, 
       xlab="Time", ylab="Miles Travelled", 
       main=paste("Rolling", time.interval/60, "min avg of distance,", dow))
  lines(timeseq, rollavg, xlim=range(timeseq), ylim=range(rollavg.45m), pch=16)
}
start.date <- as.POSIXct("2016-08-022")
end.date <- as.POSIXct("2016-10-23")
x <- distWalked[distWalked$midtime > start.date & distWalked$midtime < end.date,]
#x <- distWalked[months(distWalked$midtime) == "September" | distWalked$midtime) == "Oct" | ,]
plotavgbyDOW(x,"Wednesday")

