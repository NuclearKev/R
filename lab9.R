## These arrays are used for graphing and the legend
colors <- c("antiquewhite4",
            "burlywood4",
            "black",
            "azure4",
            "chocolate3",
            "chartreuse3",
            "blueviolet",
            "blue",
            "cadetblue3",
            "darkgoldenrod1")

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

## I don't use this one
getGroups <- function(allData){
  groups <- paste(unique(allData["GroupID"])$GroupID)
  return(Map(function(group){ 
    return(subset(allData, GroupID == group))
  }, groups))
}

splitGroups <- function (allData) {
    allGroups <- list(## A = subset(allData, GroupID=="A"),
                      ## B = subset(allData, GroupID=="B"),
                      ## C = subset(allData, GroupID=="C"),
                      ## D = subset(allData, GroupID=="D"),
                      ## E = subset(allData, GroupID=="E"),
                      L = subset(allData, GroupID=="L"),
                      J = subset(allData, GroupID=="J"))
                      ## K = subset(allData, GroupID=="K"),
                      ## M = subset(allData, GroupID=="M"),
                      ## N = subset(allData, GroupID=="N"))
    return(allGroups)
}

getMembers <- function(groups) {
  Map(function(group) {
    groupMembers <- paste(unique(group["BoardID"])$BoardID)
    return(Map(function(member){ 
      return(subset(group, BoardID == member))
    }, groupMembers))
  }, groups)
}

singleGroupGraph <- function (member, connector) {
    xlab = "Input Voltage (V)"
    ylab = "Output Voltage (V)"

    down <- subset(member, JconnectorID == connector &
                          UpDown == "down")
    up <- subset(member, JconnectorID == connector &
                        UpDown == "up")
    
    xlim <- c(min(up$VoltageIn), max(up$VoltageIn))
    ylim <- c(min(up$VoltageOut), max(up$VoltageOut))

    graph <- plot(x=down$VoltageIn, type = "p", xlim = xlim, ylim = ylim,
                  xlab = xlab, ylab = ylab,
                  main = "Output Voltage vs Input Voltage")

    lines(y = down$VoltageOut, x = down$VoltageIn, col = "red",
          type = "p", pch = 19)
    lines(y = up$VoltageOut, x = up$VoltageIn, col = "blue",
          type = "p", pch = 19)

    return("Success!")
}
