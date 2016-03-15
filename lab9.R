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
splitGroups <- function (allData) {
    allGroups <- list(## groupA = subset(allData, GroupID=="A"),
                      ## groupB = subset(allData, GroupID=="B"),
                      ## groupC = subset(allData, GroupID=="C"),
                      ## groupD = subset(allData, GroupID=="D"),
                      ## groupE = subset(allData, GroupID=="E"),
                      groupJ = subset(allData, GroupID=="J"))
                      ## groupK = subset(allData, GroupID=="K"),
                      ## groupL = subset(allData, GroupID=="L"),
                      ## groupM = subset(allData, GroupID=="M"),
                      ## groupN = subset(allData, GroupID=="N"))
    return(allGroups)
}

singleGroupGraph <- function (allData, groupID, connector) {

    xlab = "Input Voltage (V)"
    ylab = "Output Voltage (V)"

    person1Down <- subset(allData, GroupID == groupID &
                                      JconnectorID == connector &
                                      UpDown == "down")
    person1Up <- subset(allData, GroupID == groupID &
                                 JconnectorID == connector &
                                 UpDown == "up")
    
    xlim <- c(min(person1Up$VoltageIn), max(person1Up$VoltageIn))
    ylim <- c(min(person1Up$VoltageOut), max(person1Up$VoltageOut))

    graph <- plot(x=person1Down$VoltageIn, type = "p", xlim = xlim, ylim = ylim,
                  xlab = xlab, ylab = ylab,
                  main = "Output Voltage vs Input Voltage")

    lines(y = person1Down$VoltageOut, x = person1Down$VoltageIn, col = "red",
          type = "p", pch = 19)
    lines(y = person1Up$VoltageOut, x = person1Up$VoltageIn, col = "blue",
          type = "p", pch = 19)

    return("Success!")
}
