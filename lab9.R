## These arrays are used for graphing and the legend
add.alpha <- function(col, alpha=1){
  if(missing(col))
      stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, function(x) 
        rgb(x[1], x[2], x[3], alpha=alpha))  
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

# Splits the members in every group expects you to have called getGroups 
# before, output in form of:
# var$groupID$member
getMembers <- function(groups) {
  Map(function(group) {
    groupMembers <- paste(unique(group["BoardID"])$BoardID)
    return(Map(function(member){ 
      return(subset(group, BoardID == member))
    }, groupMembers))
  }, groups)
}

# Gets the members from all data; output in form of:
# members$member
flatMembers <- function(allData){
  members <- paste(unique(allData["BoardID"])$BoardID)
  return(Map(function(member){ 
    return(subset(allData, BoardID == member))
  }, members))
}

# Graphs a single connector
singleGroupGraph <- function (member, connector) {
    xlab = "Input Voltage (V)"
    ylab = "Output Voltage (V)"

    down <- subset(member, JconnectorID == connector &
                          UpDown == "down")
    up <- subset(member, JconnectorID == connector &
                        UpDown == "up")
    
    graph <- plot(x=NULL,
                  y=NULL,
                  type = "p",
                  xlim = c(0, 3.3), 
                  ylim = c(0, 3.3),
                  xlab = xlab,
                  ylab = ylab,
                  main = paste("Output Voltage vs Input Voltage (", 
                               connector, ")", sep=""))

    lines(y = down$VoltageOut, x = down$VoltageIn, col = colors[1],
          type = "p", pch = 19)
    lines(y = up$VoltageOut, x = up$VoltageIn, col = colors[5],
          type = "p", pch = 19)
    lines(y = down$VoltageOut, x = down$VoltageIn, col = colors[2],
          type = "l", pch = 19)
    lines(y = up$VoltageOut, x = up$VoltageIn, col = colors[2],
          type = "l", pch = 19)
    #From datasheet
    lines(y = c(0,3.3), x = c(vILDataSheet, vILDataSheet), col = colors[8],
          type = "l", pch = 19)
    lines(y = c(0,3.3), x = c(vIHDataSheet, vIHDataSheet), col = colors[8],
          type = "l", pch = 19)

  names <- c("Down", "Up");
  legend(0, 3.3, names, col=c(colors[1], colors[5]), pch=19)

  return("Success!")
}

vILDataSheet <- 0.800
vIHDataSheet <- 2.000

getVIL <- function(member, connector){
  low <- subset(member, JconnectorID == connector & UpDown == "down" & 
                 VoltageOut < 0.4)
  return(max(low$VoltageIn))
}
getVIH <- function(member, connector){
  high <- subset(member, JconnectorID == connector & UpDown == "up" & 
                 VoltageOut > 2.9)
  return(min(high$VoltageIn))
}

getHyst <- function(member, connector){
  down <- subset(member, JconnectorID == connector & UpDown == "down" & 
                 VoltageOut > 2.9)
  down <- min(down$VoltageIn)
  up <- getVIH(member, connector)
  return(up-down)
}

# gets all VIL for the member
getAllVIL <- function(member){
  connectors <- paste(unique(member["JconnectorID"])$JconnectorID)
  return(Map(function(conn) getVIL(member, conn), connectors))
}
# gets all VIH for the member
getAllVIH <- function(member){
  connectors <- paste(unique(member["JconnectorID"])$JconnectorID)
  return(Map(function(conn) getVIH(member, conn), connectors))
}

getAllHyst <- function(member){
  connectors <- paste(unique(member["JconnectorID"])$JconnectorID)
  return(Map(function(conn) getHyst(member, conn), connectors))
}

# Cleans up the getAllVIL
mapAllVIL <- function(member){
  vils <- getAllVIL(member)
  realVILS <- Filter(function(vil) vil != -Inf & vil != Inf, vils)
  return(do.call(cbind, realVILS))
}
# Cleans up the getAllVIH
mapAllVIH <- function(member){
  vihs <- getAllVIH(member)
  realVIHS <- Filter(function(vih) vih != -Inf & vih != Inf, vihs)
  return(do.call(cbind, realVIHS))
}

# Gets the mean VIL for member
getMeanVIL <- function(member){
  vils <- mapAllVIL(member)
  m <- mean(vils)
  return(m)
}
# Gets the mean VIH for member
getMeanVIH <- function(member){
  vils <- mapAllVIH(member)
  m <- mean(vils)
  return(m)
}

getPercentErrorsVIL <- function(member){
  vil <- getAllVIL(member)
  vil <- do.call(cbind, vil)
  return(abs(vil-vILDataSheet)/vILDataSheet)
}
getPercentErrorsVIH <- function(member){
  vil <- getAllVIH(member)
  vil <- do.call(cbind, vil)
  return(abs(vil-vIHDataSheet)/vIHDataSheet)
}

# Get the standard deviation for member
getSdVIL <- function(member){
  return(sd(mapAllVIL(member)))
}
getSdVIH <- function(member){
  return(sd(mapAllVIH(member)))
}

# gets all VIL for all members
getVILAllMembers <- function(allMembers){
  Map(function(member) getAllVIL(member), allMembers)
}
# gets all VIH for all members
getVIHAllMembers <- function(allMembers){
  Map(function(member) getAllVIH(member), allMembers)
}
getAllV <- function(allMembers){
  return(Map(function(member){
    return(list(VIL = getAllVIL(member),
                VIH = getAllVIH(member)))
  }, allMembers))
}

getAllMeansVIL <- function(allMembers){
  Map(function(member) getMeanVIL(member), allMembers)
}
getAllMeansVIH <- function(allMembers){
  Map(function(member) getMeanVIH(member), allMembers)
}
getAllMeans <- function(allMembers){
  return(Map(function(member){
    return(list(VIL = getMeanVIL(member),
                HIL = getMeanVIH(member)))
  }, allMembers))
}

getAllSdVIL <- function(allMembers){
  Map(function(member) getSdVIL(member), allMembers)
}
getAllSdVIH <- function(allMembers){
  Map(function(member) getSdVIL(member), allMembers)
}
getAllSd <- function(allMembers){
  return(Map(function(member){
    return(list(VIL = getSdVIL(member),
                HIL = getSdVIH(member)))
  }, allMembers))
}

getAllHystClass <- function(allMembers){
  return(Map(function(member){
    return(HIL = getAllHyst(member))
  }, allMembers))
}

getAllError <- function(allMembers){
  return(Map(function(member){
    return(list(VIL = getPercentErrorsVIL(member),
                VIH = getPercentErrorsVIH(member)))
  }, allMembers))
}

graphAllGroups <- function (members, connector, dir) { #dir is direction

  xlab = "Input Voltage (V)"
  ylab = "Output Voltage (V)"

  getThisGuySomeKnots <- Map(function(member){
    return(subset(member, JconnectorID==connector & UpDown==dir))
  }, members)

  graph <- plot(x=NULL,
                y=NULL,
                type = "p",
                xlim = c(0, 3.3), 
                ylim = c(0, 3.3),
                xlab = xlab,
                ylab = ylab,
                main = paste("Output Voltage vs Input Voltage for ",
                             connector, " (", dir, ")", sep=""))
  i = 1
  for (mem in getThisGuySomeKnots) {
    lines(y = mem$VoltageOut, x = mem$VoltageIn, type = "p",
          col = colors[i], pch = 19)
    i = i + 1
  }

  lines(y = c(0,3.3), x = c(vILDataSheet, vILDataSheet), col = colors[8],
        type = "l", pch = 19)
  lines(y = c(0,3.3), x = c(vIHDataSheet, vIHDataSheet), col = colors[8],
        type = "l", pch = 19)


  return("Success!")
}
