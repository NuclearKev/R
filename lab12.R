## These arrays are used for graphing and the legend
add.alpha <- function(col, alpha=1){
  if(missing(col))
      stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, function(x) 
        rgb(x[1], x[2], x[3], alpha=alpha))  
}

setAs("character","myDate", function(from) strptime(from, format="%m/%d/%Y %H:%M") )

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

fixDates <- function(allData){
    return(cbind(allData, DateTime=Map(function(date){
        return (strptime(date, format="%m/%d/%Y %H:%M"))
    }, allData$DateTime)))
}

## Use getGroups before running this function
graphSingleGroup <- function (group, testNumber = 1) {
    xlab <- "Time";
    ylab <- "Temperature (F)"

    bulbTempData <- subset(group, TestNumber == testNumber);

    graph <- plot(x=NULL,
                  y=NULL,
                  type = "p",
                  xlim = c(0, 3.3),
                  ylim = c("4/7/2016 10:00", "4/8/2016 11:00"),
                  xlab = xlab,
                  ylab = ylab,
                  main = paste("Output Voltage vs Input Voltage (", 
                               connector, ")", sep=""))

    lines(y = bulbTempData$BulbTemperatureDegF, x = bulbTempData$DateTime, col = colors[1],
          type = "p", pch = 19)

    return("Success!");
}

getSD <- function(group, test){
  data <- subset(group, TestNumber == test)
  return(sd(data$BulbTemperatureDegF))
}

getMean <- function(group, test){
  data <- subset(group, TestNumber == test)
  return(mean(data$BulbTemperatureDegF))
}

getMax <- function(group, test){
  data <- subset(group, TestNumber == test)
  return(max(data$BulbTemperatureDegF))
}

getMin <- function(group, test){
  data <- subset(group, TestNumber == test)
  return(min(data$BulbTemperatureDegF))
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
  return(ab)
}
 
