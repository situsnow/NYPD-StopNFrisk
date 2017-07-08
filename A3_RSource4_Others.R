#import necessary library
#1. For scatter plot
library(reshape2)
library(lattice)
library(ggplot2)
library(AppliedPredictiveModeling)
library(caret)


###############################################################################
# 8. find out the percetage of physical force against different races while they 
#are suspected to commit the same crime
phyForcePercentage = data.frame(matrix(ncol = 13, nrow = 0))
colNames <- c("RaceId", "RaceDesp", "CrimeId", "CrimeDesp", "Hands", "OnGround", "AgainstWall", 
             "WeaponDrawn", "WeaponPointed", "Baton",
             "Handcuffs", "PepperSpray", "Other")
colnames(phyForcePercentage) <- colNames

#loop the percentage of seven races and 113 crimes
for(i in 1:7){
  for(j in 1:113){
    raceCriteria = stopfrisked3_arule$race == i
    crimeCriteria = stopfrisked3_arule$detailcm == j
    temp.df = stopfrisked3_arule[raceCriteria & crimeCriteria, ]
    if (nrow(temp.df) > 0){
      length_temp.df <- nrow(temp.df)
      
      race <- raceDesp[raceDesp$race == i,2]
      crime <- crmCdDataFrame[crmCdDataFrame$detailcm == j,2]
      
      pf_hands_p <- nrow(temp.df[temp.df$pf_hands == 1,])/length_temp.df
      pf_wall_p <- nrow(temp.df[temp.df$pf_wall == 1,])/length_temp.df
      pf_grnd_p <- nrow(temp.df[temp.df$pf_grnd == 1,])/length_temp.df
      pf_drwep_p <- nrow(temp.df[temp.df$pf_drwep == 1,])/length_temp.df
      pf_ptwep_p <- nrow(temp.df[temp.df$pf_ptwep == 1,])/length_temp.df
      pf_baton_p <- nrow(temp.df[temp.df$pf_baton == 1,])/length_temp.df
      pf_hcuff_p <- nrow(temp.df[temp.df$pf_hcuff == 1,])/length_temp.df
      pf_pepsp_p <- nrow(temp.df[temp.df$pf_pepsp == 1,])/length_temp.df
      pf_other_p <- nrow(temp.df[temp.df$pf_other == 1,])/length_temp.df
      
      flag <- pf_hands_p > 0 | pf_wall_p > 0 | pf_grnd_p > 0 | pf_drwep_p > 0 | pf_ptwep_p > 0 | 
        pf_baton_p > 0 | pf_hcuff_p > 0 | pf_pepsp_p > 0 | pf_other_p > 0
      
      if(flag){
        phyForcePercentage[nrow(phyForcePercentage) + 1, ] <- c(i, race, j, crime, 
                                                               pf_hands_p, pf_wall_p, pf_grnd_p, pf_drwep_p, pf_ptwep_p, pf_baton_p, pf_hcuff_p, pf_pepsp_p, pf_other_p)
      }
    }
  }
}

phyForcePercentage$RaceId <- as.factor(phyForcePercentage$RaceId)
phyForcePercentage$RaceDesp <- as.factor(phyForcePercentage$RaceDesp)
phyForcePercentage$CrimeId <- as.factor(phyForcePercentage$CrimeId)
phyForcePercentage$CrimeDesp <- as.factor(phyForcePercentage$CrimeDesp)

phyForcePercentage$Hands <- as.numeric(phyForcePercentage$Hands)
phyForcePercentage$AgainstWall <- as.numeric(phyForcePercentage$AgainstWall)
phyForcePercentage$OnGround <- as.numeric(phyForcePercentage$OnGround)

phyForcePercentage$WeaponDrawn <- as.numeric(phyForcePercentage$WeaponDrawn)
phyForcePercentage$WeaponPointed <- as.numeric(phyForcePercentage$WeaponPointed)
phyForcePercentage$Baton <- as.numeric(phyForcePercentage$Baton)

phyForcePercentage$Handcuffs <- as.numeric(phyForcePercentage$Handcuffs)
phyForcePercentage$PepperSpray <- as.numeric(phyForcePercentage$PepperSpray)
phyForcePercentage$Other <- as.numeric(phyForcePercentage$Other)


#Density Class
#We can review the density distribution of each attribute broken down by class value. 
#Like the scatterplot matrix, the density plot by class can help see the separation of classes. 
#It can also help to understand the overlap in class values for an attribute.

# x-axis is the actual value, say the possibility of different features
# y-axis is the distribution rate of inner class (races in this circumstance)

x <- phyForcePercentage[, 5:13]
y <- phyForcePercentage[, 2]
#x = phyForcePercentage[phyForcePercentage$CrimeId == 20, 5:13]
#y = phyForcePercentage[phyForcePercentage$CrimeId == 20, 2]

transparentTheme(trans = .9)
scales <- list(x = list(relation = "free"), y = list(relation = "free"))

featurePlot(x = x, y = y, plot = "density", scales = scales, 
            adjust = 1.5, pch = "|", layout = c(3, 3), 
            main = "The desity of police force among races",
            auto.key = list(columns = 7),
            par.settings = list(
              superpose.line = list(col = c("Green", "Pink", "Black", "Blue", "Orange", "Red","Brown"), 
                                    lty=c(1,1,1,1,1,1,1))))

###############################################################################
# 9. find out the percetage of reasons to stop against different races while they 
#are suspected to commit the same crime

stopReasonPercentage <- data.frame(matrix(ncol = 14, nrow = 0))
colNames <- c("RaceId", "RaceDesp", "CrimeId", "CrimeDesp", "SuspiciousObj", "FitDesp", 
             "CasVictimLoc", "ActLookout", "CrimeClothes", "DrugActions", "FurtiveMove", 
             "ViolentAction", "SuspBulge", "Other")
colnames(stopReasonPercentage) <- colNames

#loop the percentage of seven races and 113 crimes
for(i in 1:7){
  for(j in 1:113){
    raceCriteria <- stopfrisked3_arule$race == i
    crimeCriteria <- stopfrisked3_arule$detailcm == j
    temp.df <- stopfrisked3_arule[raceCriteria & crimeCriteria, ]
    if (nrow(temp.df) > 0){
      length_temp.df <- nrow(temp.df)
      
      race <- raceDesp[raceDesp$race == i,2]
      crime <- crmCdDataFrame[crmCdDataFrame$detailcm == j,2]
      
      cs_objcs_p <- nrow(temp.df[temp.df$cs_objcs == 1, ])/length_temp.df
      cs_descr_p <- nrow(temp.df[temp.df$cs_descr == 1, ])/length_temp.df
      cs_casng_p <- nrow(temp.df[temp.df$cs_casng == 1, ])/length_temp.df
      cs_lkout_p <- nrow(temp.df[temp.df$cs_lkout == 1, ])/length_temp.df
      cs_cloth_p <- nrow(temp.df[temp.df$cs_cloth == 1, ])/length_temp.df
      cs_drgtr_p <- nrow(temp.df[temp.df$cs_drgtr == 1, ])/length_temp.df
      cs_furtv_p <- nrow(temp.df[temp.df$cs_furtv == 1, ])/length_temp.df
      cs_vcrim_p <- nrow(temp.df[temp.df$cs_vcrim == 1, ])/length_temp.df
      cs_bulge_p <- nrow(temp.df[temp.df$cs_bulge == 1, ])/length_temp.df
      cs_other_p <- nrow(temp.df[temp.df$cs_other == 1, ])/length_temp.df
      
      
      flag <- cs_objcs_p > 0 | cs_descr_p > 0 | cs_casng_p > 0 | cs_lkout_p > 0 | cs_cloth_p |
        cs_drgtr_p > 0 | cs_furtv_p > 0 | cs_vcrim_p > 0 | cs_bulge_p > 0 | cs_other_p > 0
      
      if(flag){
        stopReasonPercentage[nrow(stopReasonPercentage) + 1, ] <- 
          c(i, race, j, crime, cs_objcs_p, cs_descr_p, cs_casng_p, cs_lkout_p, cs_cloth_p,
            cs_drgtr_p, cs_furtv_p, cs_vcrim_p, cs_bulge_p, cs_other_p)
      }
    }
  }
}

stopReasonPercentage$RaceId <- as.factor(stopReasonPercentage$RaceId)
stopReasonPercentage$RaceDesp <- as.factor(stopReasonPercentage$RaceDesp)
stopReasonPercentage$CrimeId <- as.factor(stopReasonPercentage$CrimeId)
stopReasonPercentage$CrimeDesp <- as.factor(stopReasonPercentage$CrimeDesp)

stopReasonPercentage$SuspiciousObj <- as.numeric(stopReasonPercentage$SuspiciousObj)
stopReasonPercentage$FitDesp <- as.numeric(stopReasonPercentage$FitDesp)
stopReasonPercentage$CasVictimLoc <- as.numeric(stopReasonPercentage$CasVictimLoc)
stopReasonPercentage$ActLookout <- as.numeric(stopReasonPercentage$ActLookout)
stopReasonPercentage$CrimeClothes <- as.numeric(stopReasonPercentage$CrimeClothes)

stopReasonPercentage$DrugActions <- as.numeric(stopReasonPercentage$DrugActions)
stopReasonPercentage$FurtiveMove <- as.numeric(stopReasonPercentage$FurtiveMove)
stopReasonPercentage$ViolentAction <- as.numeric(stopReasonPercentage$ViolentAction)
stopReasonPercentage$SuspBulge <- as.numeric(stopReasonPercentage$SuspBulge)
stopReasonPercentage$Other <- as.numeric(stopReasonPercentage$Other)

x <- stopReasonPercentage[, 5:14]
y <- stopReasonPercentage[, 2]

#transparentTheme(trans = .9)
scales <- list(x = list(relation = "free"), y = list(relation = "free"))

featurePlot(x = x, y = y, plot = "density", scales = scales, 
            adjust = 1.5, pch = "|", layout = c(5, 2), 
            main = "The desity of stop reasons among races",
            auto.key = list(columns = 7),
            par.settings = list(
              superpose.line = list(col = c("Green", "Pink", "Black", "Blue", "Orange", "Red","Brown"), 
                                    lty=c(1,1,1,1,1,1,1))))