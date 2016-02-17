# Second dataset: instead of summarizing time-of-day features into one row, keep each epoch.
# In this case, each row in the new dataset will be <PID, Day, Time-of-day, simple_features>
# Simple features: A1.AVG, A1.STD, A1.CV, Steps

# Outline:
#     1. Dataset Creation
#     2. Excluding outliers
#     3. Clustering epochs into k clusters - checking each cluster center and labeling them meaningful words
#     4. Aggregate for participants <W_{k, morning}: proportion of W_k in the mornings for one participant>
#     5. Random Forest for feature and prediction evaluation

# This script covers the step (1)
library(PhysicalActivity)
source("~/Workspaces/R workspace/Epoch Analysis/epoch_analysis/Circadian Analysis/f03_epoch_dataset_creation_functions.R")

# Temporary for easier file selection
setwd("~/../../Volumes/aging/SHARE/ARRC/Active_Studies/ANALYSIS_ONGOING/LIFE Main data/LIFE accelerometry - second data - 10_26_15/")


# Select PID_VC_HID
clr()
load("PID_VC_HID.Rdata")

valid.files <- valid_participants(PID_VC_HID = REF, valid.days = 5)
rm(REF)


output.df <- data.frame(matrix(nrow = 0, ncol = 8))
colnames(output.df) <- c("PID", "Day", "Time", "a1.avg", "a1.std", "a1.cv", "a1.energy", "steps")

for (i in 1:nrow(valid.files)) {
     PID <- valid.files$pid[i]
     HID <- paste("HID", valid.files$HID[i], ".RData", sep = "")
     load(HID)
     
     wearTimes.info <- find.wearTime(AC.1s)
     
     clr()
     print(paste(i, " out of ", nrow(valid.files), " - Being processed... ", HID, " (PID: ", PID, ")", sep = ""))
     participant.df <- main.constructEpochDataset(AC.1s, wearTimes.info, PID)
     output.df <- data.frame(rbind(output.df, participant.df))
}

# Saving the data into a csv file ------------------------------
setwd("~/R Codes/01 Bag of Words/02 Circadian Analysis/")
write.csv(output.df, file = "d01_epoch_and_transitions_021716.csv", row.names = F)
rm(list = ls())


