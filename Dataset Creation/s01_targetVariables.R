# This script finds all possible target variables for epoch analysis project.

setwd("~/Dropbox/Work-Research/Current Directory/Bag of Words/epoch_analysis/Dataset Creation/")

library(haven)

maskid_accpid <- read_dta("../../Datasets/Target Variables - Key Features/ACCPID to LIFE maskid.dta")
keyFeatures.df <- read_dta("../../Datasets/Target Variables - Key Features/baseline_key_variables_v2_2.dta")

features <- c("MaskID", "gender_tscr", "livalone_demg", "bmi", "smoking",
              "healthrt_mhah", "sub_diabetes", "arthrits_mhah", "lungdis_mhah",
              "totalscore_chmp", "gait_speed_sppb", "sub_gait", "tot_scr_sppb", "sub_sppb", "walk_comp_w400", "walk_time",
              "mobility_disq", "total_score_disq", "_3MSE", "sub_3MSE", "MetS")

keyFeatures.df <- keyFeatures.df[, features]

# Marking Factor Variables
keyFeatures.df$livalone_demg <- factor(keyFeatures.df$livalone_demg) # No: 0, Yes: 1
keyFeatures.df$smoking <- factor(keyFeatures.df$smoking) # Never: 0, Former: 1, Current: 2
keyFeatures.df$healthrt_mhah <- factor(keyFeatures.df$healthrt_mhah) # Excellent: 1, Poor: 5 | Missing: 6-7-8
keyFeatures.df$sub_diabetes <- factor(keyFeatures.df$sub_diabetes) # No: 0, Impaired Fasting Glucose: 1, Diabetes: 2
keyFeatures.df$arthrits_mhah <- factor(keyFeatures.df$arthrits_mhah) # No: 0, Yes: 1
keyFeatures.df$lungdis_mhah <- factor(keyFeatures.df$lungdis_mhah) # No: 0, Yes: 1, Possible: 2
keyFeatures.df$sub_gait <- factor(keyFeatures.df$sub_gait) # 0.8+ : 0, <0.8: 1
keyFeatures.df$sub_sppb <- factor(keyFeatures.df$sub_sppb) # 8-9: 0, 1-7: 1
keyFeatures.df$walk_comp_w400 <- factor(keyFeatures.df$walk_comp_w400) # No: 0, Yes: 1
keyFeatures.df$mobility_disq <- keyFeatures.df$mobility_disq # 1: No, 5: Yes (1-2 | 3-5)
keyFeatures.df$sub_3MSE <- factor(keyFeatures.df$sub_3MSE) # 90+: 0, <90: 1
keyFeatures.df$MetS <- factor(keyFeatures.df$MetS) # No: 0, Yes: 1
rm(features)

# Selecting participants with >= 5 valid days ####
accelerometerFeatures.df <- read_dta("../../Datasets/Target Variables - Key Features/life10_accelerometry_v2_1.dta")
# Selecting baseline data
accelerometerFeatures.df <- accelerometerFeatures.df[accelerometerFeatures.df$vc == "SV1", ]
# Selecting participants with 5+ valid days
valid.maskIds <- accelerometerFeatures.df[accelerometerFeatures.df$valid_days > 4, "MaskID"]
rm(accelerometerFeatures.df)

keyFeatures.df <- keyFeatures.df[keyFeatures.df$MaskID %in% valid.maskIds$MaskID, ]
pids <- sapply(keyFeatures.df$MaskID, function(x) {maskid_accpid$accpid[which(maskid_accpid$maskid == x)]})
keyFeatures.df$accpid <- factor(pids, levels = pids)
rm(pids)

# saving the selected target variables for later analysis
save(file = "../../Datasets/Target Variables - Key Features/out01_baseline_selected_targetVariables_020716.RData", keyFeatures.df)

rm(list = ls())

