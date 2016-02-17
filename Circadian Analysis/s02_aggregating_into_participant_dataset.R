# Simple aggregation of participant's time-of-day data (15-sec)

# A participant has many rows (mornining-..._evening for several days).
# This script constructs one row for each participant
library(haven)
library(rpart)
setwd("~/Workspaces/R workspace/Epoch Analysis/epoch_analysis/")
source("Circadian Analysis/f02_feature_aggregation.R")

color.code <- c("red", "blue", "green", "black")

# FIFTEEN-SECOND EPOCH DATASET -----------------------------------------------------------------
clr()
print("Select your dataset:")
epoch.df <- read.csv(file.choose())
epoch.df$PID <- factor(epoch.df$PID)

# Outlier Detection - Epoch ===================================================
epoch.df <- replace_NA_with_zero(epoch.df)
outlier.idx <- outlier.detection(epoch.df, exclude.column = c(11, 12), count.threshold = 2, value.threshold = 3)
epoch.df <- epoch.df[-outlier.idx, ]
rm(outlier.idx)


# Simple Aggregation ==========================================================
aggregated.df <- data.frame(matrix(nrow = 0, ncol = 41))
colnames(aggregated.df) <- c("PID",
                             "morning.a1.avg", "morning.a1.std", "morning.a1.energy", "morning.a1.locomotion.avg", "morning.a1.locomotion.energy", "morning.steps", "morning.locomotion", "morning.vm.avg", "morning.vm.std", "morning.vm.energy",
                             "noon.a1.avg", "noon.a1.std", "noon.a1.energy", "noon.a1.locomotion.avg", "noon.a1.locomotion.energy", "noon.steps", "noon.locomotion", "noon.vm.avg", "noon.vm.std", "noon.vm.energy",
                             "afternoon.a1.avg", "afternoon.a1.std", "afternoon.a1.energy", "afternoon.a1.locomotion.avg", "afternoon.a1.locomotion.energy", "afternoon.steps", "afternoon.locomotion", "afternoon.vm.avg", "afternoon.vm.std", "afternoon.vm.energy",
                             "evening.a1.avg", "evening.a1.std", "evening.a1.energy", "evening.a1.locomotion.avg", "evening.a1.locomotion.energy", "evening.steps", "evening.locomotion", "evening.vm.avg", "evening.vm.std", "evening.vm.energy")
for(pid in levels(epoch.df$PID)) {
     oneParticipant.df <- epoch.df[epoch.df$PID == pid, ]
     aggregated.df <- data.frame(rbind(aggregated.df, simple_aggregate_one_participant.15sec(oneParticipant.df)))
}
rm(pid, oneParticipant.df)
aggregated.df <- replace_NA_with_zero(aggregated.df)
save(aggregated.df, file = "~/Dropbox/Work-Research/Current Directory/Bag of Words/Datasets/Outputs - Circadian Analysis/d02_participants_simple_aggregated_0211616.RData")


# Outlier Detection - Participant ==============================================
outlier.idx <- outlier.detection(aggregated.df, exclude.column = 1, count.threshold = 3, value.threshold = 3)
aggregated.df <- aggregated.df[-outlier.idx, ]
rm(outlier.idx)


# Check Different Target Variables =============================================

clr()
print("Select the target variable files: >>>>  Full_Target_Variables_021616.RData  <<<<")
load(file.choose())
load(file = "~/Dropbox/Work-Research/Current Directory/Bag of Words/Datasets/Checkpoints/circadianV1_epoch_aggregate_checkpoint_01.RData")

# Check Point #################################
# This checkpoint is used for other dataset constructions
save.image(file = "~/Dropbox/Work-Research/Current Directory/Bag of Words/Datasets/Checkpoints/circadianV1_epoch_aggregate_checkpoint_01.RData")

# Change target variable
target.var <- ordered.targetVariable(aggregated.df$PID, target.df$accpid, target.df$walkspeed < 0.8)

exclude.idx <- which(is.na(target.var$targetVar))
examine.df <- data.frame(aggregated.df[-exclude.idx, ], class = factor(target.var$targetVar[-exclude.idx]))
rm(exclude.idx)
examine.df[, -c(1, 42)] <- scale(examine.df[, -c(1, 42)])

# Divide training and test set
set.seed(5855)
training.idx <- sample.int(nrow(examine.df), size = nrow(examine.df))
test.idx <- training.idx[1:floor(nrow(examine.df) * 0.2)]
training.idx <- training.idx[-test.idx]

parallel.plot(examine.df[, -42], targetVar = examine.df$class)

# Checking the outcome
tree.data <- examine.df[, -c(1)]
tree.fit <- rpart(class~., data = tree.data, control = rpart.control(minsplit = 40, minbucket = 20))
table(tree.data$class, predict(tree.fit, examine.df[, -42], type = "class"))
plot(tree.fit, margin = 0.1, compress = T, uniform = T)
text(tree.fit, use.n = T)





# Labling method ======================================================
rm(list = ls())
load("~/Dropbox/Work-Research/Current Directory/Bag of Words/Datasets/Checkpoints/circadianV1_epoch_aggregate_checkpoint_01.RData")

# Constructing label dataset based on Axis1.ActivityCount
epoch.label.df <- data.frame(PID = epoch.df$PID, time.of.day = epoch.df$transition, word = label.on.value(epoch.df, 1))
label.a1.df <- data.frame(matrix(nrow = 0, ncol = 17))
colnames(label.a1.df) <- c("PID",
"morning.mostInactive", "morning.inactive", "morning.active", "morning.mostActive",
"noon.mostInactive", "noon.inactive", "noon.active", "noon.mostActive",
"afternoon.mostInactive", "afternoon.inactive", "afternoon.active", "afternoon.mostActive",
"evening.mostInactive", "evening.inactive", "evening.active", "evening.mostActive")
for(pid in levels(epoch.label.df$PID)) {
     oneParticipant.df <- epoch.label.df[epoch.label.df$PID == pid, ]
     label.a1.df <- data.frame(rbind(label.a1.df, label.aggregate.oneParticipant(oneParticipant.df)))
}
rm(pid, oneParticipant.df)
save(epoch.df, epoch.label.df, label.a1.df, file = "~/Dropbox/Work-Research/Current Directory/Bag of Words/Datasets/Outputs - Circadian Analysis/d02_participants_a1_labels_0211616.RData")

# Checking the output #######################
target.var <- ordered.targetVariable(label.a1.df$PID, target.df$accpid, target.df$walkspeed < 0.8)

exclude.idx <- which(is.na(target.var$targetVar))
examine.df <- data.frame(label.a1.df[-exclude.idx, ], class = factor(target.var$targetVar[-exclude.idx]))
rm(exclude.idx)

# Divide training and test set
set.seed(5855)
training.idx <- sample.int(nrow(examine.df), size = nrow(examine.df))
test.idx <- training.idx[1:floor(nrow(examine.df) * 0.2)]
training.idx <- training.idx[-test.idx]

parallel.plot(examine.df[, -18], targetVar = examine.df$class)

# Checking the outcome
tree.data <- examine.df[, -c(1)]
tree.fit <- rpart(class~., data = tree.data, control = rpart.control(minsplit = 40, minbucket = 20))
table(tree.data$class, predict(tree.fit, examine.df[, -42], type = "class"))
plot(tree.fit, margin = 0.2, compress = T, uniform = T)
text(tree.fit, use.n = T)




# Constructing label dataset based on Axis1.ActivityCount at Locomotions
rm(list = ls())
load("~/Dropbox/Work-Research/Current Directory/Bag of Words/Datasets/Checkpoints/circadianV1_epoch_aggregate_checkpoint_01.RData")

epoch.label.df <- data.frame(PID = epoch.df$PID, time.of.day = epoch.df$transition, word = label.on.value(epoch.df, 4))
label.locomotion.df <- data.frame(matrix(nrow = 0, ncol = 17))
colnames(label.locomotion.df) <- c("PID",
                           "morning.mostInactive", "morning.inactive", "morning.active", "morning.mostActive",
                           "noon.mostInactive", "noon.inactive", "noon.active", "noon.mostActive",
                           "afternoon.mostInactive", "afternoon.inactive", "afternoon.active", "afternoon.mostActive",
                           "evening.mostInactive", "evening.inactive", "evening.active", "evening.mostActive")
for(pid in levels(epoch.label.df$PID)) {
     oneParticipant.df <- epoch.label.df[epoch.label.df$PID == pid, ]
     label.locomotion.df <- data.frame(rbind(label.locomotion.df, label.aggregate.oneParticipant(oneParticipant.df)))
}
rm(pid, oneParticipant.df)
for (i in 1:ncol(label.locomotion.df)) {
     print(which(is.na(label.locomotion.df[, i])))
}
label.locomotion.df <- label.locomotion.df[-c(790, 836, 891, 1184), ]
rm(i)
save(epoch.df, epoch.label.df, label.locomotion.df, file = "~/Dropbox/Work-Research/Current Directory/Bag of Words/Datasets/Outputs - Circadian Analysis/d02_participants_locomotion_labels_0211616.RData")


# Checking the output #######################
target.var <- ordered.targetVariable(label.locomotion.df$PID, target.df$accpid, target.df$walkspeed < 0.8)

exclude.idx <- which(is.na(target.var$targetVar))
examine.df <- data.frame(label.locomotion.df[-exclude.idx, ], class = factor(target.var$targetVar[-exclude.idx]))
rm(exclude.idx)

# Divide training and test set
set.seed(5855)
training.idx <- sample.int(nrow(examine.df), size = nrow(examine.df))
test.idx <- training.idx[1:floor(nrow(examine.df) * 0.2)]
training.idx <- training.idx[-test.idx]

parallel.plot(examine.df[, -18], targetVar = examine.df$class)

# Checking the outcome
tree.data <- examine.df[, -c(1)]
tree.fit <- rpart(class~., data = tree.data, control = rpart.control(minsplit = 40, minbucket = 20))
table(tree.data$class, predict(tree.fit, examine.df[, -42], type = "class"))
plot(tree.fit, margin = 0.2, compress = T, uniform = T)
text(tree.fit, use.n = T)

# Constructing label dataset based on steps
rm(list = ls())
load("~/Dropbox/Work-Research/Current Directory/Bag of Words/Datasets/Checkpoints/circadianV1_epoch_aggregate_checkpoint_01.RData")

epoch.label.df <- data.frame(PID = epoch.df$PID, time.of.day = epoch.df$transition, word = label.on.value(epoch.df, 5))
label.steps.df <- data.frame(matrix(nrow = 0, ncol = 17))
colnames(label.steps.df) <- c("PID",
                           "morning.mostInactive", "morning.inactive", "morning.active", "morning.mostActive",
                           "noon.mostInactive", "noon.inactive", "noon.active", "noon.mostActive",
                           "afternoon.mostInactive", "afternoon.inactive", "afternoon.active", "afternoon.mostActive",
                           "evening.mostInactive", "evening.inactive", "evening.active", "evening.mostActive")
for(pid in levels(epoch.label.df$PID)) {
     oneParticipant.df <- epoch.label.df[epoch.label.df$PID == pid, ]
     label.steps.df <- data.frame(rbind(label.steps.df, label.aggregate.oneParticipant(oneParticipant.df)))
}
rm(pid, oneParticipant.df)

for (i in 1:ncol(label.steps.df)) {
     print(which(is.na(label.steps.df[, i])))
}
label.locomotion.df <- label.steps.df[-c(790, 836, 891, 1184), ]
rm(i)
save(epoch.df, epoch.label.df, label.steps.df, file = "~/Dropbox/Work-Research/Current Directory/Bag of Words/Datasets/Outputs - Circadian Analysis/d02_participants_steps_labels_0211616.RData")

# Checking the output #######################
target.var <- ordered.targetVariable(label.steps.df$PID, target.df$accpid, target.df$walkspeed < 0.8)

exclude.idx <- which(is.na(target.var$targetVar))
examine.df <- data.frame(label.steps.df[-exclude.idx, ], class = factor(target.var$targetVar[-exclude.idx]))
rm(exclude.idx)

# Divide training and test set
set.seed(5855)
training.idx <- sample.int(nrow(examine.df), size = nrow(examine.df))
test.idx <- training.idx[1:floor(nrow(examine.df) * 0.2)]
training.idx <- training.idx[-test.idx]

parallel.plot(examine.df[, -18], targetVar = examine.df$class)

# Checking the outcome
tree.data <- examine.df[, -c(1)]
tree.fit <- rpart(class~., data = tree.data, control = rpart.control(minsplit = 40, minbucket = 20))
table(tree.data$class, predict(tree.fit, examine.df[, -42], type = "class"))
plot(tree.fit, margin = 0.2, compress = T, uniform = T)
text(tree.fit, use.n = T)



