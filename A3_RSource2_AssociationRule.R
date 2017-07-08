#import necessary library
#1. For mongodb
library(devtools)
library(rmongodb)

#2. For Association Rule
library(Matrix)
library(arules)
#3. visualize association rules
library(kernlab)
library(grid)
library(arulesViz)

####################################################################
#Join the two table (stopfrisked1 and stopfrisked2 by unique key)

###NOTE:
#mongo.aggregation is NOT supporting syntax $lookup with current rmongodb version (released in 2011) 
#where $lookup is released in MongoDB 3.2 (2015)
#this pipeline is working aggRule <- mongo.bson.from.list(list("$match" = list("age" = list("$gte" = 30))))
#this pipeline is NOT working aggRule <- mongo.bson.from.list(list("$lookup" = list("from" = ns2, "localField" = "Id", "foreignField" = "Id", "as" = ns2)))

#1. Aggregate in mongodb shell, and new a collection to save the aggregation result
#db.stopfrisked1.aggregate([{$lookup:{from: "stopfrisked2", localField: "Id", foreignField: "Id", as: "stopfrisked2"}}, {$out:"stopfrisked3"}])

#2. Export this new collection
#./bin/mongoexport --db fit5141 --collection stopfrisked3 --type=csv --fieldFile ./data/FieldFile.txt --out ./data/stopfrisked3.csv

#3. Import into R workspace
stopfrisked3 <- read.csv("~/MIT Course Documents/FIT5141_Advanced Topics in Information Technology/Assignment3/Dataset/stopfrisked3.csv")

#4. Rename those column names as fields inside array will not be in right format

for(oriColName in names(stopfrisked3)){
  if(!is.na(pmatch("stopfrisked2.0.", oriColName))){
    newName <- substr(oriColName, 16, nchar(oriColName))
    names(stopfrisked3)[names(stopfrisked3) == oriColName] = newName
  }
}

####################################################################
#Connect Mongo
mongo <- mongo.create(host = "127.0.0.1", db = "fit5141")

#Rename the field name before retrieve records (There's space in field name for crime code) in Mongo Shell
#db.stopfriskedCrCd.updateMany({}, {$rename:{'Crime Codes':"CrimeCodes", "Crime Description":"CrimeDescription"}})

#Get crime code from collection stopfriskedCrCd
#new data frame to save all crime code records
crmCdDataFrame <- data.frame(matrix(ncol = 2, nrow = 0))
colNames <- c("detailcm", "CrimeDescription")
colnames(crmCdDataFrame) <- colNames

if(mongo.is.connected(mongo) == TRUE){
  ns = "fit5141.stopfriskedCrCd"
  allRecordCursor <- mongo.find(mongo, ns)
  while(mongo.cursor.next(allRecordCursor)){
    record <- list(mongo.bson.to.list(mongo.cursor.value(allRecordCursor)))
    crimeCode <- record[[1]]$CrimeCodes
    crimeDesp <- record[[1]]$CrimeDescription
    crmCdDataFrame[nrow(crmCdDataFrame) + 1,] <- c(crimeCode, crimeDesp)
  }
}

#Construct race desciption and merge to stopfrisked3_obv
raceDesp <- data.frame(matrix(ncol = 2, nrow = 0))
colNames <- c("race", "RaceDesp")
colnames(raceDesp) <- colNames

raceDesp[nrow(raceDesp)+1, ] <- c("1", "Black")
raceDesp[nrow(raceDesp)+1, ] <- c("2", "Black Hispanic")
raceDesp[nrow(raceDesp)+1, ] <- c("3", "White Hispanic")
raceDesp[nrow(raceDesp)+1, ] <- c("4", "White")
raceDesp[nrow(raceDesp)+1, ] <- c("5", "ASP Islander")
raceDesp[nrow(raceDesp)+1, ] <- c("6", "Am.IN Native")
raceDesp[nrow(raceDesp)+1, ] <- c("7", "Unknown")
###############################################################################
#Find the association rules of different characters of suspects and the result of 
#arrested/Not arrested
#6. Define the function to find association rules between different factors
findAssoRule = function(df, suppVal, confVal, rhsVec){
  assoRules <- apriori(df,control = list(verbose = F),
                      parameter = list(minlen = 1, supp = suppVal, conf = confVal),
                      appearance = list(default = "lhs", rhs = rhsVec))
  assoRules.sorted <- sort(assoRules, by = "lift")
  
  # find redundant rules
  # is.subset(), For rules, the union of lhs and rhs is used at the set of items.
  subset.matrix <- is.subset(assoRules.sorted, assoRules.sorted)
  subset.matrix[lower.tri(subset.matrix, diag = T)] = NA
  redundantRules <- colSums(subset.matrix, na.rm = T) >= 1
  #which(redundantRules)
  
  # remove redundant rules
  assoRules.pruned <- assoRules.sorted[!redundantRules]
  #inspect(assoRules.pruned)
  return(assoRules.pruned)
}

################################################################################################
#remove unnessarity field and make all related field as factor before association rule performs
stopfrisked3_arule <- subset(stopfrisked3, select = -c(Id, arstoffn, crimsusp))
#merge the race description and crime description
stopfrisked3_arule <- merge(stopfrisked3_arule, raceDesp, by = "race")
stopfrisked3_arule <- merge(stopfrisked3_arule, crmCdDataFrame, by="detailcm")

#make every attributes as vector
stopfrisked3_arule[] <- lapply(stopfrisked3_arule, factor)
colNames <- names(stopfrisked3_arule)
stopfrisked3_arule[colNames] <- lapply(stopfrisked3_arule[colNames], factor)

###############################################################################
#7. Find association rules between races and suspects characters
# rules with rhs containing "Arrested" only with below factor columns
# race, frisked, searched, contrabn, pistol, riflshot, asltweap, knifcuti, othrweap, arstmade
stopfrisked3_arule_suspChar <- 
  subset(stopfrisked3_arule, select = c(race, frisked, searched, contrabn, pistol, 
                                        riflshot, asltweap, knifcuti, othrweap, arstmade))

#Select only race = 1 (Black) and all others to compare
blackRace <- stopfrisked3_arule_suspChar$race == 1
nonBlackRace <- stopfrisked3_arule_suspChar$race != 1

stopfrisked3_arule_suspChar_b <- stopfrisked3_arule_suspChar[blackRace, ]
stopfrisked3_arule_suspChar_nb <- stopfrisked3_arule_suspChar[nonBlackRace, ]

susCharResult <- c("arstmade=1")
###############################
#for black race

rules_suspChar_b <- findAssoRule(stopfrisked3_arule_suspChar_b, 0.005, 0.8, susCharResult)

#plot(rules_suspChar_b)
plot(rules_suspChar_b, method = "graph", control = list(type = "items"))

plot(rules_suspChar_b, method = "paracoord", control = list(reorder=T))

###############################
#for non-black race
rules_suspChar_nb <- findAssoRule(stopfrisked3_arule_suspChar_nb, 0.005, 0.8, susCharResult)

#plot(rules_suspChar_nb)
plot(rules_suspChar_nb, method = "graph", control = list(type = "items"))

plot(rules_suspChar_nb, method = "paracoord", control = list(reorder=T))
