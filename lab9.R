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

## Split up the groups for easy grabbing!
splitGroups <- function (allData) {
    allGroups <- list(groupA = subset(allData, GroupID=="A"),
                      groupB = subset(allData, GroupID=="B"),
                      groupC = subset(allData, GroupID=="C"),
                      groupD = subset(allData, GroupID=="D"),
                      groupE = subset(allData, GroupID=="E"),
                      groupJ = subset(allData, GroupID=="J"),
                      groupK = subset(allData, GroupID=="K"),
                      groupL = subset(allData, GroupID=="L"),
                      groupM = subset(allData, GroupID=="M"),
                      groupN = subset(allData, GroupID=="N"))
    return(allGroups)
}

singleGroupGraph <- function (allGroupsSplit, groupID == "J") {

    xlab="Input Voltage (V)"
    ylab="Output Voltage (V)"

    

    
}
