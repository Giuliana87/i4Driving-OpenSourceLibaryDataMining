################data event################
#####processing data and MCA #############


#set working directory
setwd("~/setuserwd")

#read data event
DataEvent <- read.csv("DataEvent.txt", sep=";", comment.char="#", stringsAsFactors=TRUE)

#descriptive statistics
cnames <- colnames(DataEvent)
natvar <- numeric(length=ncol(DataEvent))
#check kind of variables
for (i in 1: length(natvar)){
  natvar[i] <- ifelse(is.factor(DataEvent[,i]),1,0) #1 factor variable, 0 otherwise
}

############preprocessing variables
#packages required
require(papeR)
require(xtable)
require(export)
require(FactoMineR)
require(factoextra)
require(rockchalk)
DEvent <- DataEvent
cnames2 <- cnames
#recode variable's names
for (j in 2:length(natvar)){
  if (nchar(cnames[j])>31){
    cnames2[j] <- substring(cnames2[j],(nchar(cnames2[j])-29) ) 
  }
}

#fill missing data with NA
for (j in 2:length(natvar)){
  if (natvar[j]==1){
    DEvent[,j] <- replace(DEvent[,j],which(DEvent[,j]==""),NA)
  }
}

#check missing numbers
missit <- numeric(length=142) 
for (i in 1:142){
  missit[i] <- sum(is.na(DEvent[,i]))
}

#prepare data matrix for Multiple Correspondence Analysis (MCA)
idmiss <- which(missit <= 32579)
idmiss2 <- which(missit == 0)
idmiss <- idmiss[-1]
idmiss2 <- idmiss2[-1]
require(FactoMineR)
cnames[idmiss]
cnames[idmiss2]
DE4mca <- DEvent[,idmiss2]
DE4mca$EventID <- DataEvent$EventID
idcat <- which(natvar[idmiss]==1)
idcat2 <- which(natvar[idmiss2]==1)
DE4mcaold <- DE4mca
DE4mca <- DE4mca[,idcat2]
numbcat <- numeric(length=ncol(DE4mca))
for (j in 1:length(numbcat)){
  numbcat[j] <- length(unique(DE4mca[,j]))
}

colnames(DE4mca[,c(4,5,6,24,25,26)])
summary(DE4mca[,c(4,5,6,24,25,26)])

require(crunch)
DE4mca2 <- DE4mca 
DE4mca2 <- DE4mca2[,-c(5,6,7,25,26)]
colnames(DE4mca2)


#merge categories with few observations or reduce number of categories
cutpoint <- 50
#processing Pre Incindent Maneuver   1 -> 23  (1 -> 23 significa la prima variabile diventa la 23)
DE4mca2$PreInciManeuver <- combineLevels(DE4mca2$Pre_IncidentManeuver, c(6,7,8), "Going straight (combined)")
othercat <- which(table(DE4mca2$PreInciManeuver)<cutpoint)
DE4mca2$PreInciManeuver <- combineLevels(DE4mca2$PreInciManeuver, othercat, "Other (combined)")

#processing Maneuver Judgment  2 -> 2
sort(table(DE4mca2$ManeuverJudgment))



#processing Driving Behavior 1    4 -> 24
behcat <- which(table(DE4mca2$DrivingBehavior1)<110)
DE4mca2$DriveBehav1 <- combineLevels(DE4mca2$DrivingBehavior1, behcat, "Other (combined)")
levels(DE4mca2$DriveBehav1)[2] <- "slowly-not below speed limit"
levels(DE4mca2$DriveBehav1)[3] <- "slowly-below speed limit"
levels(DE4mca2$DriveBehav1)[4] <- "drowsy"
levels(DE4mca2$DriveBehav1)[5] <- "exceeded safe speed"
levels(DE4mca2$DriveBehav1)[9] <- "Improper backing"
DE4mca2$DriveBehav1 <- combineLevels(DE4mca2$DriveBehav1, c(10,11), "improper turn(combined)")
levels(DE4mca2$DriveBehav1)[11] <- "Right-of-way error"
levels(DE4mca2$DriveBehav1)[12] <- "Stop sign violation"

#processing HandsOnWheel     5 -> 25
table(DE4mca2$HandsOnWheel)
handcat <- levels(DE4mca2$HandsOnWheel)
DE4mca2$HandsWheel <- combineLevels(DE4mca2$HandsOnWheel, c(2,3,4), "left hand(combined)")
DE4mca2$HandsWheel <- combineLevels(DE4mca2$HandsWheel, c(4,5,6), "right hand(combined)")
DE4mca2$HandsWheel <- combineLevels(DE4mca2$HandsWheel, c(2,3), "none(combined)")
levels(DE4mca2$HandsWheel)[1] <- "HandsWheel_Both hands"
levels(DE4mca2$HandsWheel)[2] <- "HandsWheel_Unknown"
levels(DE4mca2$HandsWheel)[3] <- "HandsWheel_left hand(combined)"
levels(DE4mca2$HandsWheel)[4] <- "HandsWheel_right hand(combined)"
levels(DE4mca2$HandsWheel)[5] <- "HandsWheel_none(combined)"




#processing DriverSeatbelt    6 -> 26
table(DE4mca2$DriverSeatbelt)
beltcat <- levels(DE4mca2$DriverSeatbelt)
DE4mca2$DrivSet <- combineLevels(DE4mca2$DriverSeatbelt, c(3,4,5), "none/unknown(combined)")
levels(DE4mca2$DrivSet)[1] <- "belt NOT properly worn"
levels(DE4mca2$DrivSet)[2] <- "belt properly worn"

#processing Light    7 -> 7
table(DE4mca2$Light)
levels(DE4mca2$Light)[1] <- "Darkness(lighted)"
levels(DE4mca2$Light)[2] <- "Darkness(not-lighted)"

#processing Weather    8 -> 27
table(DE4mca2$Weather)
weathcat <- levels(DE4mca2$Weather)
DE4mca2$Weather2 <- combineLevels(DE4mca2$Weather, c(6,7,9), "other(combined)")
DE4mca2$Weather2 <- combineLevels(DE4mca2$Weather2, c(1,4), "fog/rain_fog(combined)")

#processing SurfaceCndtn    9 -> 28
table(DE4mca2$SurfaceCndtn)
DE4mca2$SurfCond <- combineLevels(DE4mca2$SurfaceCndtn, c(2,3,4,5,6,8), "other(combined)")

#processing TrafficFlow    10 -> 10
table(DE4mca2$TrafficFlow)
#levels(x)[3] <- "three"
levels(DE4mca2$TrafficFlow)[1] <- "Divided"
levels(DE4mca2$TrafficFlow)[3] <- "Not Divided(center 2-way)"
levels(DE4mca2$TrafficFlow)[4] <- "Not Divided(simple 2-way)"

#processing ContiguousTrvlLanes    11 -> 29
table(DE4mca2$ContiguousTrvlLanes)
DE4mca2$ConTrvLanes <- combineLevels(DE4mca2$ContiguousTrvlLanes, c(8,9,10), "7+")

#processing ThruTrvlLanes    12 -> 30
table(DE4mca2$ThruTrvlLanes)
DE4mca2$ThrTrvLanes <- combineLevels(DE4mca2$ThruTrvlLanes, c(6,7,8,9), "5+")

#processing V1LaneOccupied    13 -> 31
table(DE4mca2$V1LaneOccupied)
DE4mca2$V1LaneOcc <- combineLevels(DE4mca2$V1LaneOccupied, c(5,6,7), "5+")
levels(DE4mca2$V1LaneOcc)[8] <- "left turn lane"
levels(DE4mca2$V1LaneOcc)[9] <- "right turn lane"

#processing TrafficDensity    14 -> 14
table(DE4mca2$TrafficDensity)
levels(DE4mca2$TrafficDensity)[1] <- "Level A1"
levels(DE4mca2$TrafficDensity)[2] <- "Level A2"
levels(DE4mca2$TrafficDensity)[3] <- "Level B"
levels(DE4mca2$TrafficDensity)[4] <- "Level C"
levels(DE4mca2$TrafficDensity)[5] <- "Level D"
levels(DE4mca2$TrafficDensity)[6] <- "Level E"
levels(DE4mca2$TrafficDensity)[7] <- "Level F"

#processing TrafficControl    15 -> 32
table(DE4mca2$TrafficControl)
DE4mca2$TrafContr <- combineLevels(DE4mca2$TrafficControl, c(2,4,5,6), "Othe(combined)")
DE4mca2$TrafContr <- combineLevels(DE4mca2$TrafContr, c(3,4,5), "Railroad(combined)")
levels(DE4mca2$TrafContr)[1] <- "Construction signs"
levels(DE4mca2$TrafContr)[3] <- "School zone"
levels(DE4mca2$TrafContr)[4] <- "Slow/warning sign"
levels(DE4mca2$TrafContr)[6] <- "Traffic lanes"

#processing RelationToJunction    16 -> 33
table(DE4mca2$RelationToJunction)
DE4mca2$RelToJunc <- combineLevels(DE4mca2$RelationToJunction, c(7,10), "Other")
levels(DE4mca2$RelToJunc)[1] <- "Driveway"
levels(DE4mca2$RelToJunc)[7] <- "Parking_entrance/exit"
levels(DE4mca2$RelToJunc)[8] <- "Parking_within boundary"

#processing RdAlignment    17 -> 34
table(DE4mca2$RdAlignment)
DE4mca2$RdAlign <- combineLevels(DE4mca2$RdAlignment, c(3,5), "Other")

#processing Grade    18 -> 18
table(DE4mca2$Grade)

#processing Locality    19 -> 19
table(DE4mca2$Locality)
levels(DE4mca2$Locality)[1] <- "Business"
levels(DE4mca2$Locality)[2] <- "Bypass"
levels(DE4mca2$Locality)[4] <- "Interstate"

#processing IntersectionInfluence    20 -> 20
table(DE4mca2$IntersectionInfluence)
levels(DE4mca2$IntersectionInfluence)[3] <- "Yes(Interchange)"
levels(DE4mca2$IntersectionInfluence)[4] <- "Yes(Other)"
levels(DE4mca2$IntersectionInfluence)[5] <- "Yes(Parking lot)"
levels(DE4mca2$IntersectionInfluence)[6] <- "Yes(Stop sign)"
levels(DE4mca2$IntersectionInfluence)[7] <- "Yes(Traffic sign)"
levels(DE4mca2$IntersectionInfluence)[8] <- "Yes(Uncontrolled)"

#processing SecTask1    21 -> 35
table(DE4mca2$SecTask1)
DE4mca2$STask1 <- combineLevels(DE4mca2$SecTask1, c(7:16), "Cell phone(combined)")
DE4mca2$STask1 <- combineLevels(DE4mca2$STask1, c(7,8), "Child interaction(combined)")
DE4mca2$STask1 <- combineLevels(DE4mca2$STask1, c(10:13), "Drinking(combined)")
DE4mca2$STask1 <- combineLevels(DE4mca2$STask1, c(1:3, 14), "Adjusting car devices(combined)")
DE4mca2$STask1 <- combineLevels(DE4mca2$STask1, c(1:4, 30, 35), "Personal care(combined)")
DE4mca2$STask1 <- combineLevels(DE4mca2$STask1, c(3,4, 24), "Eating(combined)")
DE4mca2$STask1 <- combineLevels(DE4mca2$STask1, c(3,5,21,27), "Smoking(combined)")
DE4mca2$STask1 <- combineLevels(DE4mca2$STask1, c(16,17), "Passenger Interaction(combined)")
DE4mca2$STask1 <- combineLevels(DE4mca2$STask1, c(19,20,21), "Removing/adjusting personal stuffs(combined)")
DE4mca2$STask1 <- combineLevels(DE4mca2$STask1, c(2,4,5,6,7,12), "External distractions(combined)")
DE4mca2$STask1 <- combineLevels(DE4mca2$STask1, c(13:16, 21), "Cell phone/tablet(combined)")
DE4mca2$STask1 <- combineLevels(DE4mca2$STask1, c(2,5,10,12,14,15,16), "Other(combined)")
DE4mca2$STask1 <- combineLevels(DE4mca2$STask1, c(5,20), "Other(combined)")
levels(DE4mca2$STask1)[2] <- "Moving object"
levels(DE4mca2$STask1)[4] <- "Object in vehicle"
levels(DE4mca2$STask1)[7] <- "Reaching for object"
levels(DE4mca2$STask1)[8] <- "Talking/singing"

#processing ConstructionZone    22 -> 22
table(DE4mca2$ConstructionZone)
levels(DE4mca2$ConstructionZone)[1] <- "Construction zone-related"
levels(DE4mca2$ConstructionZone)[2] <- "Construction Zone"
levels(DE4mca2$ConstructionZone)[3] <- "Not construction zone"

#processing Impairments   direttamente 36
DE4mca2$Impairments <- combineLevels(DE4mca$Impairments, c(2,6,8,15,9,10,16,17), "Drug/alcohol related(combined)")
DE4mca2$Impairments <- combineLevels(DE4mca2$Impairments, c(1,2), "Angry(combined)")
DE4mca2$Impairments <- combineLevels(DE4mca2$Impairments, c(1,2,3), "Drowsy(combined)")
DE4mca2$Impairments <- combineLevels(DE4mca2$Impairments, c(1,2,4,5,6), "Other(combined)")
levels(DE4mca2$Impairments)[1] <- "None apparent"
levels(DE4mca2$Impairments)[2] <- "Drug/alcohol(combined)"


####DATA FOR MCA with ID EVENT
indcol <- c(23,2,3,24,25,26,7,27,28,10,29,30,31,14,32,33,34,18,19,20,35,36)
DE4mcagood <- DE4mca2[,indcol]
colnames(DE4mcagood)

DE4mcagood$EventID <- DataEvent$EventID

##adjust some levels
levels(DE4mcagood$HandsWheel)[1] <- "HandsWheel_Both hands"
levels(DE4mcagood$HandsWheel)[2] <- "HandsWheel_Unknown"
levels(DE4mcagood$HandsWheel)[3] <- "HandsWheel_left hand(combined)"
levels(DE4mcagood$HandsWheel)[4] <- "HandsWheel_right hand(combined)"
levels(DE4mcagood$HandsWheel)[5] <- "HandsWheel_none(combined)"

levels(DE4mcagood$DrivSet)[1] <- "DriverSseat_belt-NOT-properly worn"
levels(DE4mcagood$DrivSet)[2] <- "DriverSseat_belt properly worn"
levels(DE4mcagood$DrivSet)[3] <- "DriverSseat_none/unknown(combined)"

levels(DE4mcagood$Light)[1] <- "Light_Darkness(lighted)"
levels(DE4mcagood$Light)[2] <- "Light_Darkness(not-lighted)"
levels(DE4mcagood$Light)[3] <- "Light_Dawn"
levels(DE4mcagood$Light)[4] <- "Light_Daylight"
levels(DE4mcagood$Light)[5] <- "Light_Dusk"

levels(DE4mcagood$TrafficFlow)[1] <- "TrafficFlow_Divided"
levels(DE4mcagood$TrafficFlow)[2] <- "TrafficFlow_No lanes"
levels(DE4mcagood$TrafficFlow)[3] <- "TrafficFlow_Not Divided(center-2-way)"
levels(DE4mcagood$TrafficFlow)[4] <- "TrafficFlow_Not Divided(simple-2-way)"
levels(DE4mcagood$TrafficFlow)[5] <- "TrafficFlow_One-way-traffic"

levels(DE4mcagood$Grade)[1] <- "Grade_Dip"
levels(DE4mcagood$Grade)[2] <- "Grade_Down"
levels(DE4mcagood$Grade)[3] <- "Grade_Up"
levels(DE4mcagood$Grade)[4] <- "Grade_Hillcrest"
levels(DE4mcagood$Grade)[5] <- "Grade_Level"

#MCA
res.mca = MCA(DE4mcagood, quali.sup=3, graph=FALSE, ncp = 8)
p <- ncol(DE4mcagood)
#Benzecri correction
ls <- res.mca$eig[,1][which(res.mca$eig[,1]>(1/p))]
lstar <- (p/(p-1))^2 * (ls - (1/p))^2
lstar <- lstar/sum(lstar)
cuminertia <- cumsum(lstar)
#plots
require(factoextra)
require(ggpubr)
options(ggrepel.max.overlaps = 20) #defaul 10

var12 <- 
  fviz_mca_var(res.mca, repel = TRUE,
               ggtheme= theme_minimal(), 
               col.var = "contrib",
               gradient.cols = c("darkgray", "blue", "red"),
               xlab=paste("Dim1: (",round(lstar[1]*100,3),"%)"),
               ylab=paste("Dim2: (",round(lstar[2]*100,3),"%)"),
               col.quali.sup="black")
#select.var = list(contrib = 60) )

quartz()
var34 <- 
  fviz_mca_var(res.mca, repel = TRUE,
               ggtheme= theme_minimal(), 
               col.var = "contrib",
               axes=c(3,4),
               gradient.cols = c("darkgray", "blue", "red"),
               xlab=paste("Dim3: (",round(lstar[3]*100,3),"%)"),
               ylab=paste("Dim4: (",round(lstar[4]*100,3),"%)"),
               col.quali.sup="black")
#select.var = list(contrib = 60) )


quartz()
var56 <- 
  fviz_mca_var(res.mca, repel = TRUE,
               ggtheme= theme_minimal(), 
               col.var = "contrib",
               axes=c(5,6),
               gradient.cols = c("darkgray", "blue", "red"),
               xlab=paste("Dim5: (",round(lstar[5]*100,3),"%)"),
               ylab=paste("Dim6: (",round(lstar[6]*100,3),"%)"),
               col.quali.sup="black")
#select.var = list(contrib = 60) )

var78 <- 
  fviz_mca_var(res.mca, repel = TRUE,
               ggtheme= theme_minimal(), 
               col.var = "contrib",
               axes=c(7,8),
               gradient.cols = c("darkgray", "blue", "red"),
               xlab=paste("Dim7: (",round(lstar[7]*100,3),"%)"),
               ylab=paste("Dim8: (",round(lstar[8]*100,3),"%)"),
               col.quali.sup="black")

ggexport(var12, width=800, height=600,
         filename = "MCA1.png")
ggexport(var34, width=800, height=600,
         filename = "MCA2.png")
ggexport(var56, width=800, height=600,
         filename = "MCA3.png")
ggexport(var78, width=800, height=600,
         filename = "MCA4.png")

quartz()
fviz_mca_var(res.mca, repel = TRUE, 
             ggtheme= theme_minimal(), axes=c(3,4))

quartz()
fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = 3, # color by groups 
             #alpha.ind=0.5,
             palette = c("lightgray","darkgray","black","blue","green","purple"),
             #addEllipses = TRUE, ellipse.type = "confidence",
             pointsize=0.9,
             ggtheme = theme_minimal(),
             xlab=paste("Dim1: (",round(lstar[1]*100,3),"%)"),
             ylab=paste("Dim2: (",round(lstar[2]*100,3),"%)")) 

quartz()
fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = 3, # color by groups 
             axes=c(3,4),
             palette = c("lightgray","darkgray","black","blue","green","purple"),
             pointsize=0.9,
             #addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal(),
             xlab=paste("Dim3: (",round(lstar[3]*100,3),"%)"),
             ylab=paste("Dim4: (",round(lstar[4]*100,3),"%)")) 


fviz_mca_ind(res.mca, 
             label = "none", # hide individual labels
             habillage = 3, # color by groups 
             axes=c(5,6),
             palette = c("lightgray","darkgray","black","blue","green","purple"),
             pointsize=0.9,
             #addEllipses = TRUE, ellipse.type = "confidence",
             ggtheme = theme_minimal(),
             xlab=paste("Dim5: (",round(lstar[5]*100,3),"%)"),
             ylab=paste("Dim6: (",round(lstar[6]*100,3),"%)")) 

xtable(res.mca$var$cos2)

var <- get_mca_var(res.mca)
cntrb1 <- sort(var$contrib[,1],decreasing=TRUE)
cntrb2 <- sort(var$contrib[,2],decreasing=TRUE)
cntrb3 <- sort(var$contrib[,3],decreasing=TRUE)
cntrb4 <- sort(var$contrib[,4],decreasing=TRUE)
cbind(cntrb1[1:10],cntrb2[1:10],cntrb3[1:10],cntrb4[1:10])
ind1 <- order(var$contrib[,1],decreasing=TRUE)
ind2 <- order(var$contrib[,2],decreasing=TRUE)
ind3 <- order(var$contrib[,3],decreasing=TRUE)
ind4 <- order(var$contrib[,4],decreasing=TRUE)

row.names(var$contrib)[ind1[1:10]]
row.names(var$contrib)[ind2[1:10]]
row.names(var$contrib)[ind3[1:10]]
row.names(var$contrib)[ind4[1:10]]



cntrb1[1:10]
cntrb2[1:10]
cntrb3[1:10]
cntrb4[1:10]


bestcos21 <- var$cos2[which(var$cos2[,1]>=0.3),1]
bestcos22 <- var$cos2[which(var$cos2[,2]>=0.3),2]
bestcos23 <- var$cos2[which(var$cos2[,3]>=0.3),3]
bestcos24 <- var$cos2[which(var$cos2[,4]>=0.3),4]
bestcos25 <- var$cos2[which(var$cos2[,5]>=0.3),5]
bestcos26 <- var$cos2[which(var$cos2[,6]>=0.3),6]
bestcos27 <- var$cos2[which(var$cos2[,7]>=0.3),7]
bestcos28 <- var$cos2[which(var$cos2[,8]>=0.3),8]

bestcontr1 <- var$contrib[which(var$contrib[,1]>=5),1]
bestcontr2 <- var$contrib[which(var$contrib[,2]>=5),2]
bestcontr3 <- var$contrib[which(var$contrib[,3]>=5),3]
bestcontr4 <- var$contrib[which(var$contrib[,4]>=5),4]
bestcontr5 <- var$contrib[which(var$contrib[,5]>=5),5]
bestcontr6 <- var$contrib[which(var$contrib[,6]>=5),6]
bestcontr7 <- var$contrib[which(var$contrib[,7]>=5),7]
bestcontr8 <- var$contrib[which(var$contrib[,8]>=5),8]

#export data

DE4trees <- DE4mcagood
DE4trees$EventNature1 <- DEvent$EventNature1
DE4trees$IncidentType1 <- DEvent$IncidentType1


write.table(DE4trees, "DE4trees.txt", sep=";", row.names=FALSE)
