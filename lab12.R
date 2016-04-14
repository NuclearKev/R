## These arrays are used for graphing and the legend
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, function(x) 
    rgb(x[1], x[2], x[3], alpha=alpha))  
}

setAs("character","myDate", function(from) strptime(from, format="%m/%d/%Y %H:%M") )

## Fixes issues with the datetime
readData <- function(file){
  return(read.csv(file, 
                  colClass=c('myDate', 
                             'character', 
                             'numeric', 
                             'numeric', 
                             'numeric')
                  )
         )
}

colors <- c("antiquewhite4",
            "black",
            "burlywood4",
            "azure4",
            "chocolate3",
            "chartreuse3",
            "blueviolet",
            "blue",
            "cadetblue3",
            "darkgoldenrod1")

colors <- add.alpha(colors, alpha=1.0)

names <- c("Group A", 
           "Group B", 
           "Group C", 
           "Group D",
           "Group E",
           "Group J", 
           "Group K", 
           "Group L", 
           "Group M", 
           "Group N")

                                        # Splits all the groups. Output in form of:
                                        #  var$groupID
getGroups <- function(allData){
  groups <- paste(unique(allData["GroupID"])$GroupID)
  return(Map(function(group){ 
    return(subset(allData, GroupID == group))
  }, groups))
}

## fixDates <- function(allData){
##     return(cbind(allData, DateTime=Map(function(date){
##         return (strptime(date, format="%m/%d/%Y %H:%M"))
##     }, allData$DateTime)))
## }

## Use getGroups before running this function
graphSingleGroup <- function (allGroups, group, testNumber = 1) {
  xlab <- "Time (Hour of day)"
  ylab <- "Temperature (F)"

  bulbTempData <- subset(group, TestNumber == testNumber);
  minTime <- min(bulbTempData$DateTime);
  maxTime <- max(bulbTempData$DateTime);

  roomTempData <- subset(allGroups$T, DateTime >= minTime & DateTime <= maxTime);
  minT = min(roomTempData$RoomTemperatureDegF)

  graph <- plot(y=bulbTempData$BulbTemperatureDegF,
                x=bulbTempData$DateTime,
                type = "p",
                pch = 19,
                col = colors[1],
                ylim = c(minT, 92),
                xlab = xlab,
                ylab = ylab,
                main = paste("Bulb Temperature vs Time of Day (Run ",
                             testNumber, ")", sep=""))

  lines(y = roomTempData$RoomTemperatureDegF, x = roomTempData$DateTime,
        col = colors[8], type = "p", pch = 19)

  return("Success!");
}

## In order to get the room temperatures, you want group = groupT and test = 0
## for the next few functions
getSD <- function(group, test){
  data <- subset(group, TestNumber == test)
  if(test == 0){
    return(sd(data$RoomTemperatureDegF))
  }else{
    return(sd(data$BulbTemperatureDegF))
  }
}

getMean <- function(group, test){
  data <- subset(group, TestNumber == test)
  if(test == 0){
    return(mean(data$RoomTemperatureDegF))
  }else{
    return(mean(data$BulbTemperatureDegF))
  }
}

getMax <- function(group, test){
  data <- subset(group, TestNumber == test)
  if(test == 0){
    return(max(data$RoomTemperatureDegF))
  }else{
    return(max(data$BulbTemperatureDegF))
  }
}

getGroupSomething <- function(group, f){
  return(list(a = f(group, 1),
              b = f(group, 2)))
}

getAllGroups <- function(groups, f){
  Map(function(group){
    getGroupSomething(group, f)
  }, FilterGroups(groups))
}

FilterGroups <- function(groups){
  return(Filter(function(group) group$GroupID[1]!="T", groups))
}

getAll <- function(groups){
  allMax <- getAllGroups(groups, getMax)
  allMin <- getAllGroups(groups, getMin)
  allMean <- getAllGroups(groups, getMean)
  return(list(max=allMax, min=allMin, mean=allMean))
}

getMin <- function(group, test){
  data <- subset(group, TestNumber == test)
  if(test == 0){
    return(min(data$RoomTemperatureDegF))
  }else{
    return(min(data$BulbTemperatureDegF))
  }
}

getAboveBelow <- function(group, test){
  data <- subset(group, TestNumber == test)
  ab <- Map(function(temp){
    if(temp>90){
      return(1)
    }else if(temp<90){
      return (-1)
    }else{
      return (0)
    }
  }, data$BulbTemperatureDegF)
  output = list(bulb = ab,
                datetime = data$DateTime)
  return(output)
}

#Returns on or off times in minutes
#Set onOrOff to -1 for off times
#Defualt is on time
getOnTime <- function(group, test, onOrOff = 1){
  data <- getAboveBelow(group, test)
  return(length(Filter(function(onOff) onOff == onOrOff, data$bulb))*10/60)
}
 
graphAboveBelow <- function (group, testNumber = 1) {
  xlab <- "Time"
  ylab <- "Temperature (F)"

  data <- getAboveBelow(group, testNumber)

  xlims = c(min(data$datetime), max(data$datetime))
  ylims = c(-1, 1)

  graph <- plot(x=data$datetime,
                y=data$bulb,
                type = "p",
                xlab = xlab,
                ylab = ylab,
                xlims = xlims,
                ylims = ylims,
                pch = 19,
                col = colors[1],
                main = paste("Bulb Temperature vs Time (run ", testNumber, ")", 
                             sep=""))

  return("Success!");
}
