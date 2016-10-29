# Dylan O'Connell
# Health Data Cleaning

library(XML)
xmldoc <- xmlParse("export.xml")
xmldoc <- paste(readLines('export.xml'), '\n', collapse='')
x <- xmlTreeParse(xmldoc, asText=TRUE)
x <- xmlToList(x$doc$children[[1]])
x <- x[names(x) == 'Record']

options(stringsAsFactors=FALSE)
stepCount <- list()
distWalked <- list()
flightsClimbed <- list()
for(rec in x){
  if(rec[1] == "HKQuantityTypeIdentifierStepCount"){
    stepCount <- rbind(stepCount, rec)
  }
  if(rec[1] == "HKQuantityTypeIdentifierDistanceWalkingRunning"){
    distWalked <- rbind(distWalked, rec)
  }
  if(rec[1] == "HKQuantityTypeIdentifierFlightsClimbed"){
    flightsClimbed <- rbind(flightsClimbed, rec)
  }
}

stepCount <- lapply(data.frame(stepCount),as.character)
distWalked <- lapply(data.frame(distWalked),as.character)
flightsClimbed <- lapply(data.frame(flightsClimbed),as.character)

rownames(stepCount) <- NULL
stepCount <- stepCount[!names(stepCount)=="type"]
stepCount$value<-as.numeric(stepCount$value)
stepCount$creationDate <- as.POSIXct(stepCount$creationDate, 
                              format="%Y-%m-%d %H:%M:%S %z")
stepCount$startDate <- as.POSIXct(stepCount$startDate,format="%Y-%m-%d %H:%M:%S %z")
stepCount$endDate <- as.POSIXct(stepCount$endDate,format="%Y-%m-%d %H:%M:%S %z")

rownames(distWalked) <- NULL
distWalked <- distWalked[!names(distWalked)=="type"]
distWalked$value<-as.numeric(distWalked$value)
distWalked$creationDate <- as.POSIXct(distWalked$creationDate, 
                                     format="%Y-%m-%d %H:%M:%S %z")
distWalked$startDate <- as.POSIXct(distWalked$startDate, 
                                  format="%Y-%m-%d %H:%M:%S %z")
distWalked$endDate <- as.POSIXct(distWalked$endDate, 
                                format="%Y-%m-%d %H:%M:%S %z")


rownames(flightsClimbed) <- NULL
flightsClimbed <- flightsClimbed[!names(flightsClimbed)=="type"]
flightsClimbed$value<-as.numeric(flightsClimbed$value)
flightsClimbed$creationDate <- as.POSIXct(flightsClimbed$creationDate, 
                                     format="%Y-%m-%d %H:%M:%S %z")
flightsClimbed$startDate <- as.POSIXct(flightsClimbed$startDate, 
                                  format="%Y-%m-%d %H:%M:%S %z")
flightsClimbed$endDate <- as.POSIXct(flightsClimbed$endDate, 
                                format="%Y-%m-%d %H:%M:%S %z")


stepCount$midtime <- as.POSIXct((as.numeric(stepCount$startDate) + as.numeric(stepCount$endDate)) / 2, origin = '1970-01-01')
distWalked$midtime <- as.POSIXct((as.numeric(distWalked$startDate) + as.numeric(distWalked$endDate)) / 2, origin = '1970-01-01')
flightsClimbed$midtime <- as.POSIXct((as.numeric(flightsClimbed$startDate) + as.numeric(flightsClimbed$endDate)) / 2, origin = '1970-01-01')

#stepCount$timerange <- difftime(stepCount$endDate,stepCount$startDate, units="min")
write.csv(stepCount,file="stepCount.csv", row.names = FALSE)
write.csv(distWalked,file="distWalked.csv", row.names = FALSE)
write.csv(flightsClimbed,file="flightsClimbed.csv", row.names = FALSE)


# every 5 minutes from the start until the end of our dataset
time.interval = 900
timeseq <- seq(from=range(stepCount$midtime)[1], to=range(stepCount$midtime)[2], by=time.interval)
rollavg.45m <- rep(0, length(timeseq))
time.start <- as.numeric(range(stepCount$midtime)[1])

# This as.integer will round some mid times up a half second, but this is
# all approximate
stepCount$secFromStart <- as.integer(stepCount$midtime) - time.start

for(i in 1:length(stepCount$secFromStart)){
  totstepsseq[stepCount$secFromStart[i] / time.interval ] <- totstepsseq[stepCount$secFromStart[i] / time.interval] + stepCount$value[i]
  totstepsseq[stepCount$secFromStart[i] / time.interval + 1] <- totstepsseq[stepCount$secFromStart[i] / time.interval + 1] + stepCount$value[i]
  totstepsseq[stepCount$secFromStart[i] / time.interval + 2] <- totstepsseq[stepCount$secFromStart[i] / time.interval + 2] + stepCount$value[i]
}

totstepsseq <- totstepsseq[1:length(timeseq)]
plot(timeseq,totstepsseq)





#x <- read.csv("cl_stepCount.csv", as.is=TRUE)

#lapply(list(stepCount,distWalked,flightsClimbed),function(df){
#  df <- df[,!names(df)=="type"]
#  df
#})

#for(df in list(stepCount,distWalked,flightsClimbed)){
#  rownames(df) <- NULL
#  df <- df[,!names(df)=="type"]
#  #df$sourceName <- as.factor(df$sourceName)
##  df$value<-as.numeric(df$value)
#  df$creationDate <- as.POSIXct(as.character(df$creationDate), 
#                                format="%Y-%m-%d %H:%M:%S %z")
#}

#stepCount$creationDate <- as.POSIXct(as.character(stepCount$creationDate), 
#                                     format="%Y-%m-%d %H:%M:%S %z")

#as.POSIXct("2016-10-23 10:39:28 -0400", 
#          format="%Y-%m-%d %H:%M:%S %z")
