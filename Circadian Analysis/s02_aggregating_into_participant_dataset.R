# Simple aggregation of participant's time-of-day data (15-sec)

# A participant has many rows (mornining-..._evening for several days).
# This script constructs one row for each participant
library(haven)
library(rpart)
setwd("~/Workspaces/R workspace/Epoch Analysis/epoch_analysis/")

color.code <- c("red", "blue", "green", "black")

# FIFTEEN-SECOND EPOCH DATASET -----------------------------------------------------------------
clr()
print("Select your dataset:")
epoch.df <- read.csv(file.choose())
epoch.df$PID <- factor(epoch.df$PID)

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
aggregated.df <- replace_NA_with_zero(aggregated.df)
save(aggregated.df, file = "~/Dropbox/Work-Research/Current Directory/Bag of Words/Datasets/Outputs - Circadian Analysis/d02_participants_simple_aggregated_0211616.RData")


# Check Different Target Variables =============================================

clr()
print("Select the target variable files: >>>>  Full_Target_Variables_021616.RData  <<<<")
load(file.choose())

# Change target variable
target.var <- ordered.targetVariable(aggregated.df$PID, target.df$accpid, target.df$walkspeed < 0.8)

exclude.idx <- which(is.na(target.var$targetVar))
examine.df <- data.frame(aggregated.df[-exclude.idx, ], class = factor(target.var$targetVar[-exclude.idx]))

# Divide training and test set
set.seed(5855)
training.idx <- sample.int(nrow(examine.df), size = nrow(examine.df))
test.idx <- training.idx[1:floor(nrow(examine.df) * 0.2)]
training.idx <- training.idx[-test.idx]

parallel.plot(examine.df[, -42], targetVar = examine.df$class)

tree.data <- examine.df[, -c(1)]
tree.fit <- rpart(class~., data = tree.data, control = rpart.control(minsplit = 40, minbucket = 20))
table(tree.data$class, predict(tree.fit, tree.data[, -42], type = "class"))
plot(tree.fit, margin = 0.2, compress = T, uniform = T)
text(tree.fit, use.n = T)
