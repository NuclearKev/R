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

names <- c("Group A", 
           "Group B", 
           "Group C", 
           "Group D", 
           "Group E", 
           "Group J", 
           "Group K", 
           "Group L", 
           "Group M", 
           "Group R")

graphPersonFromGroup <- function (group, mode, personNumber=1, x=1:10000, vin=1) {
  text = ""
  theory = NULL
  if(mode == "U"){
    text = "Unipolar"
    theory = getTheoreticalUnipolar()[x]
  }else{
    text = "Bipolar"
    theory = getTheoreticalBipolar()[x]
  }

  group <- subset(group, Mode==mode)

  groupMembers <- unique(group["BoardID"])

  member = groupMembers[1]$BoardID[personNumber];
  xadcData = subset(group, BoardID == member)$XADCvalue[x]
  
  if(vin==1){
    vin = subset(group, BoardID == member)$DMMvoltage[x]
  }else{
    vin = subset(group, BoardID == member)$requestedVoltage[x]
  }

  ymax = max(c(max(theory), max(xadcData)))
  ymin = min(c(min(theory), min(xadcData)))
  xmax = max(vin)
  xmin = min(vin)

  plot(x=NULL, y=NULL, type="p", ylim=c(ymin, ymax), xlim=c(xmin, xmax),
       ylab="XADC output", xlab="Vin (Volts)", 
       main=paste("Vin vs Digital Value (", text , ") - ", member, sep=""))

  lines(y=theory, x=vin, col=colors[2], type="p")
  lines(y=xadcData, x=vin, col=colors[1], type="p")

  # Don't know why I have to paste
  names <- c(paste(member), "Theoretical");
  legend(xmin, ymax, names, col=colors, pch=19)
}

getTheoreticalUnipolar <- function(){
  voltages = seq(0, 1, .0001)*(2**12)
  return(voltages)
}
getTheoreticalBipolar <- function(){
  voltages = seq(-0.5, 0.5, .0001)*(2**12)
  return(voltages)
}

getErrors <- function (groupsWithMembers, mode) {
  Map(function(group) {
    return(
      Map(function(member) {
          xadc <- subset(member, Mode == mode)$XADCvalue
          theory <- NULL
          if(mode == "U"){
            theory <- getTheoreticalUnipolar()[1:10000]
          }else{
            theory <- getTheoreticalBipolar()[1:10000]
          }
          return(abs(xadc-theory)/max(theory)*100)
        }, group)
    )
  }, groupsWithMembers)
}

maxPercentErrors <- function(errors, x=1:10000) {
  Map(function(group) {
    return(
      Map(function(member) {
          return(max(member[x]))
        }, group)
    )
  }, errors)
}

getMembers <- function(groups) {
  Map(function(group) {
        groupMembers <- paste(unique(group["BoardID"])$BoardID)
        return(Map(function(member) subset(group, BoardID == member), groupMembers))
  }, groups)
}

getGroups <- function (date) {
  groups <- list(A = subset(data, GroupID=="A"),
                 B = subset(data, GroupID=="B"),
                 C = subset(data, GroupID=="C"),
                 #Exculuding group D
                 D = subset(data, GroupID=="D"),
                 E = subset(data, GroupID=="E"),
                 J = subset(data, GroupID=="J"),
                 K = subset(data, GroupID=="K"),
                 L = subset(data, GroupID=="L"),
                 M = subset(data, GroupID=="M"),
                 R = subset(data, GroupID=="R"))
  return(groups)
}


