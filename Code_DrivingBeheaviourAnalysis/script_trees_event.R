########CLASSIFICATION TREE ON EVENT ####################

rm(list=ls(all=TRUE))


#load library
library(dplyr)
library(rpart)
library(rpart.plot)
#set wd and load data
setwd("~/setuserwd")
load("~/Data4trees_suball.Rdata")
load("~/Datadriver_and_event.RData")


#remove additional baseline - balance baseline

ind <- which(DataEvent_sub_all$EventSeverity1 == "Additional Baseline")
DataEvent_sub_all <- DataEvent_sub_all[-ind, ]
ind <- which(DataEvent_sub_all$EventSeverity1 == "Balanced-Sample Baseline")
DataEvent_sub_all <- DataEvent_sub_all[-ind, ]

#remove ID column
ind <- which(colnames(DataEvent_sub_all) == "EventID" | colnames(DataEvent_sub_all) == "ParticipantId" )
dat4tree <- DataEvent_sub_all[,-ind]
#remove  "EventNature1" and "IncidentType1"
ind <- which(colnames(dat4tree) == "IncidentType1" | colnames(dat4tree) == "EventNature1" )
dat4tree <- dat4tree[,-ind]

#recode response variable EventSeverity1
dat4tree$EventSeverity1 <- recode(DataEvent_sub_all$EventSeverity1,
                                  "Crash"="Crash",
                                  "Crash-Relevant"="Near-Crash",
                                  "Near-Crash"="Near-Crash",
                                  "Non-Subject Conflict"="Near-Crash")



#discard ConTrvLanes
ind <- which(colnames(dat4tree)== "ConTrvLanes"|colnames(dat4tree)== "HandsWheel")
dat4tree <- dat4tree[,-ind]



#processing some variable
dat4tree$ManeuverJudgment <- recode(dat4tree$ManeuverJudgment,
                                    "Unknown"="Safe and legal")

dat4tree$TrafficDensity <- recode(dat4tree$TrafficDensity,
                                  "Unknown" ="Level B")

dat4tree$RdAlign <- recode(dat4tree$RdAlign,
                                  "Other" ="Straight")

dat4tree$TrafficDensity <- recode(dat4tree$TrafficDensity,
                                  "Unknown" ="No")


####CLASSIFICATION TREES

###tree stump
tree.event0 <- rpart(EventSeverity1~., dat4tree, method="class", cp=0.05)
rpart.plot(tree.event0) 


### longer tree
tree.event1 <- rpart(EventSeverity1~., dat4tree, method="class")
rpart.plot(tree.event1, tweak=2.5)
#variable importance
tree.event1$variable.importance


##classification error
tree.pred <- as.character(predict(tree.event1, type = "class"))
table(tree.pred, dat4tree$EventSeverity1)
cm <- table(tree.pred, dat4tree$EventSeverity1)
err = 1 - (sum(diag(cm))/sum(cm))
err

##### prepare data for contingency table
Datadriver_and_event$nodo <- Datadriver_and_event$ind.tree
#node index
ind.tree <- names(table(tree.event1$where)) 
nodi_tree <- c(4, 10, 22, 23, 24, 25, 13, 14, 15)
# standard numeration terminal nodes:
Datadriver_and_event$nodo <-  as.integer(plyr::mapvalues(Datadriver_and_event$nodo, ind.tree, nodi_tree))


#Contingency table for AGE in terminal nodes

O_ROOTik <- table(Datadriver_and_event$EventSeverity1, Datadriver_and_event$AgeGrp) 
(p_ROOTik <- O_ROOTik / apply( O_ROOTik, 1, sum) )
# select a leaf
foglia = 4 
# observed value in a node
(O_ik <- table(Datadriver_and_event$EventSeverity1[Datadriver_and_event$nodo==foglia],  # esito sulle righe
               Datadriver_and_event$AgeGrp[Datadriver_and_event$nodo==foglia]) )                       # variabile gruppo di età
(p_ik <- O_ik / apply( O_ik, 1, sum) )
# marginal frequencies by row
(m_ik <- apply(O_ik, 1, sum))

# expected frequencies
(E_ik <- sweep(p_ROOTik, MARGIN=1, m_ik, `*`) )

# observed differences (normalized)
(residui <- (O_ik-E_ik)/sqrt(E_ik))
sum(residui^2)>qchisq(0.95, 5) # chi-square test

# i.e.: another leaf
foglia = 23  
(O_ik <- table(Datadriver_and_event$EventSeverity1[Datadriver_and_event$nodo==foglia],  # esito sulle righe
               Datadriver_and_event$AgeGrp[Datadriver_and_event$nodo==foglia]) )                       # variabile gruppo di età


(m_ik <- apply(O_ik, 1, sum))


(E_ik <- sweep(p_ROOTik, MARGIN=1, m_ik, `*`) )

(residui <- (O_ik-E_ik)/sqrt(E_ik))
sum(residui^2)>qchisq(0.95, 1)

### heatmap
color.palette = colorRampPalette(c("red", "white", "blue"))( 32 )
heatmap(residui, Rowv = NA, Colv = NA, scale="none", col= color.palette)

#RANDOM FOREST

dat4tree$EventSeverity1 <- as.factor(dat4tree$EventSeverity1)
rf.event <- randomForest::randomForest(EventSeverity1~., dat4tree)
#variable importance
randomForest::importance(rf.event)
