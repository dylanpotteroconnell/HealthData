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

#stepCount <- data.frame(stepCount)
stepCount <- data.frame(lapply(stepCount, as.character), stringsAsFactors=FALSE)
distWalked <- data.frame(distWalked)
flightsClimbed <- data.frame(flightsClimbed)

rownames(stepCount) <- NULL
stepCount <- stepCount[,!names(stepCount)=="type"]
stepCount$value<-as.numeric(stepCount$value)
stepCount$creationDate <- as.POSIXct(stepCount$creationDate, 
#stepCount$creationDate <- as.POSIXct(as.character(stepCount$creationDate), 
                              format="%Y-%m-%d %H:%M:%S %z")
stepCount$startDate <- as.POSIXct(stepCount$startDate, 
                                     format="%Y-%m-%d %H:%M:%S %z")
stepCount$endDate <- as.POSIXct(stepCount$endDate, 
                                  format="%Y-%m-%d %H:%M:%S %z")


rownames(distWalked) <- NULL
distWalked <- distWalked[,!names(distWalked)=="type"]
distWalked$value<-as.numeric(distWalked$value)
distWalked$creationDate <- as.POSIXct(as.character(distWalked$creationDate), 
                                     format="%Y-%m-%d %H:%M:%S %z")
distWalked$startDate <- as.POSIXct(as.character(distWalked$startDate), 
                                  format="%Y-%m-%d %H:%M:%S %z")
distWalked$endDate <- as.POSIXct(as.character(distWalked$endDate), 
                                format="%Y-%m-%d %H:%M:%S %z")


rownames(flightsClimbed) <- NULL
flightsClimbed <- flightsClimbed[,!names(flightsClimbed)=="type"]
flightsClimbed$value<-as.numeric(flightsClimbed$value)
flightsClimbed$creationDate <- as.POSIXct(as.character(flightsClimbed$creationDate), 
                                     format="%Y-%m-%d %H:%M:%S %z")
flightsClimbed$startDate <- as.POSIXct(as.character(flightsClimbed$startDate), 
                                  format="%Y-%m-%d %H:%M:%S %z")
flightsClimbed$endDate <- as.POSIXct(as.character(flightsClimbed$endDate), 
                                format="%Y-%m-%d %H:%M:%S %z")


stepCount$midtime <- as.POSIXct((as.numeric(stepCount$startDate) + as.numeric(stepCount$endDate)) / 2, origin = '1970-01-01')

write.csv(stepCount,file="cl_stepCount.csv", row.names = FALSE)
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
