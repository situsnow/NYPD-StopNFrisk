#import necessary library
#1. For mongodb
library(devtools)
library(rmongodb)

#2. For scatter plot
library(reshape2)
library(lattice)
library(ggplot2)
library(AppliedPredictiveModeling)
library(caret)

##########################################################################################
#5. check observation time and stop time for different race suspect and detail suspected crime

#only retrieve the related columns - race, crime code, per observation, per stop, arrested made
stopfrisked3_tmp <- subset(stopfrisked3, select= c(race, detailcm, perobs, perstop, arstmade))
stopfrisked3_obv <- merge(stopfrisked3_tmp, crmCdDataFrame, by="detailcm")
remove(stopfrisked3_tmp)


stopfrisked3_obv <- merge(stopfrisked3_obv, raceDesp, by = "race")

#make category fields to be factor
stopfrisked3_obv$detailcm <- as.factor(stopfrisked3_obv$detailcm)
stopfrisked3_obv$CrimeDescription <- as.factor(stopfrisked3_obv$CrimeDescription)
stopfrisked3_obv$race <- as.factor(stopfrisked3_obv$race)
stopfrisked3_obv$RaceDesp <- as.factor(stopfrisked3_obv$RaceDesp)
#################################################################
# Check the most common crimes that arrest the suspect
arrested_T <- stopfrisked3_obv$arstmade == 1
stopfrisked3_obv_arrested_T <- stopfrisked3_obv[arrested_T,]
summary(factor(stopfrisked3_obv_arrested_T$detailcm))
#Crime code 31 (Criminal Trespass) - 1780
#Crime code 20 (Criminal possession of weapon) - 1315
#Crime code 27 (Criminal possession of marihuana) - 661
#Crime code 85 (Robbery) - 610
crmCd31_20_27_85 <- stopfrisked3_obv_arrested_T$detailcm %in% c(31, 20, 27, 85)
stopfrisked3_obv_arrested_T <- stopfrisked3_obv_arrested_T[crmCd31_20_27_85,]
#drop unused levels
stopfrisked3_obv_arrested_T$detailcm <- droplevels(stopfrisked3_obv_arrested_T$detailcm)

####################################
# Check the most common crimes that NOT arrest the suspect
arrested_F <- stopfrisked3_obv$arstmade == 0
stopfrisked3_obv_arrested_F <- stopfrisked3_obv[arrested_F,]
summary(factor(stopfrisked3_obv_arrested_F$detailcm))
#Crime code 20 - 11137
#Crime code 85 - 5866
#TODO exclude others except crime code 20
crmCd20 <- stopfrisked3_obv_arrested_F$detailcm == 20
stopfrisked3_obv_arrested_F <- stopfrisked3_obv_arrested_F[crmCd20, ]
#drop unused levels
stopfrisked3_obv_arrested_F$detailcm <- droplevels(stopfrisked3_obv_arrested_F$detailcm)

remove(crmCd31_20_27_85)
remove(crmCd20)
##################################

#plot the relationship between crimes/races and the period of stop and observation for Arrested suspects
sp <- ggplot(stopfrisked3_obv_arrested_T, aes(x=perobs, y=perstop, color = RaceDesp)) + geom_point(shape=1)
sp + facet_grid(CrimeDescription ~ RaceDesp) + 
  labs(title = "Arrested suspects with time period of stop and observation") + 
  scale_x_discrete("Period of Observation") + scale_y_continuous("Period of Stop")

#plot the relationship between crimes/races and the period of stop and observation for NOT Arrested suspects
sp <- ggplot(stopfrisked3_obv_arrested_F, aes(x = perobs, y = perstop, color = RaceDesp)) + geom_path(shape = 1)

sp + facet_grid(. ~ RaceDesp) + 
  labs(title = "NOT arrested suspects with criminal possession of weapons") +
  scale_x_discrete("Period of Observation") + scale_y_continuous("Period of Stop")



