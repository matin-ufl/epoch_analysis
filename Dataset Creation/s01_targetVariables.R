# This script finds all possible target variables for epoch analysis project.

setwd("~/Dropbox/Work-Research/Current Directory/Bag of Words/epoch_analysis/Dataset Creation/")

library(haven)

keyFeatures.df <- read_dta("../../Datasets/Target Variables - Key Features/baseline_key_variables_v2_2.dta")

features <- c("MaskID", "gender_tscr", "livalone_demg", "bmi", "smoking",
              "healthrt_mhah", "sub_diabetes", "arthrits_mhah", "lungdis_mhah",
              "totalscore_chmp", "gait_speed_sppb", "sub_gait", "tot_scr_sppb", "sub_sppb", "walk_comp_w400", "walk_time",
              "mobility_disq", "total_score_disq", "_3MSE", "sub_3MSE", "MetS")

keyFeatures.df <- keyFeatures.df[, features]
rm(features)

