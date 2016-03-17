## These arrays are used for graphing and the legend
add.alpha <- function(col, alpha=1){
  if(missing(col))
      stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, function(x) 
        rgb(x[1], x[2], x[3], alpha=alpha))  
}

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

colors <- add.alpha(colors, alpha=0.1)

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

    graph <- plot(x=down$VoltageIn, 
                  y=NULL,
                  type = "p",
                  xlim = xlim, 
                  ylim = ylim,
                  xlab = xlab,
                  ylab = ylab,
                  main = "Output Voltage vs Input Voltage")

    lines(y = down$VoltageOut, x = down$VoltageIn, col = colors[1],
          type = "p", pch = 19)
    lines(y = up$VoltageOut, x = up$VoltageIn, col = colors[2],
          type = "p", pch = 19)

    return("Success!")
}
