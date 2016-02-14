library(PhysicalActivity)

setwd("~/R Codes/01 Bag of Words/02 Circadian Analysis/")
source("f01_dataset_creation_functions.R")

# Data File selections ------------------------------------------

# Temporary for easier file selection
setwd("V:/ARRC/Active_Studies/ANALYSIS_ONGOING/LIFE Main data/LIFE accelerometry - second data - 10_26_15/")

clr()
# Select PID_VC_HID
print("Select PID_VC_HID file")
load("PID_VC_HID.Rdata")

valid.files <- valid_participants(PID_VC_HID = REF, valid.days = 5)
rm(REF)

output.df <- data.frame(matrix(nrow = 0, ncol = 12))
colnames(output.df) <- c("a1.avg", "a1.std", "a1.energy", "a1.locomotion.avg", "a1.locomotion.energy",
                         "steps", "locomotion",
                         "vm.avg", "vm.std", "vm.energy", "transition", "PID")

for (i in 1:nrow(valid.files)) {
     PID <- valid.files$pid[i]
     HID <- paste("HID", valid.files$HID[i], ".RData", sep = "")
     load(HID)
     
     if(!is.na(AC.1s$axis3[1])) {
       AC.1s$VM <- sqrt((AC.1s$axis1^2) + (AC.1s$axis2^2) + (AC.1s$axis3^2))
     } else {
       AC.1s$VM <- sqrt((AC.1s$axis1^2) + (AC.1s$axis2^2))
     }
     wearTimes.info <- find.wearTime(AC.1s)
     
     clr()
     print(paste(i, " out of ", nrow(valid.files), " - Being processed... ", HID, " PID (", PID, ")", sep = ""))
     participant.df <- construct.features.at.transitions(accelerometer.1s.data = AC.1s, wearTimes.info, threshold.minute = 10 * 60)
     output.df <- data.frame(rbind(output.df, participant.df))
}

# Saving the data into a csv file ------------------------------
setwd("~/R Codes/01 Bag of Words/02 Circadian Analysis/")
write.csv(output.df, file = "d01_participants_transitions_021416.csv", row.names = F)
rm(list = ls())
