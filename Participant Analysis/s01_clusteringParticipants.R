library(ggplot2)
library(gridExtra)
library(reshape2)

# Loading data ---------------------------------------
setwd("~/Dropbox/Work-Research/Current Directory/Bag of Words/epoch_analysis/Participant Analysis/")
source("f01_functions_for_participants.R")

load(file = "../../Datasets/Epoch Dataset_Jan2016/03_participants_plus_a1AvgAc_baseline_020816.RData")
df <- participant_df
rm(participant_df)

load(file = "../../Datasets/Target Variables - Key Features/out01_baseline_selected_targetVariables_020716.RData")
target.df <- keyFeatures.df; rm(keyFeatures.df)
df <- df[df$pid %in% target.df$accpid, ]
target.df <- target.df[target.df$accpid %in% df$pid, ]

df <- df[with(data = df, order(pid)), ]
target.df <- target.df[with(data = target.df, order(accpid)), ]
target.df$walkspeed <- df$walkspeed
df$pid <- NULL; df$walkspeed <- NULL
# target.df contains all meta data and target variables
# df just contains the variables

res <- exclude.outlier(df)
df <- res$data; target.df <- target.df[-res$outlierIdx, ]
rm(res, exclude.outlier)

normal.df <- data.frame(scale(df))

# Checking correlation between features####
r <- data.frame(cor(normal.df))
r$feature <- factor(rownames(r), levels = rownames(r))
r <- melt(r)
ggplot(data = r, aes(x = variable, y = feature)) + geom_tile(aes(fill = abs(value)), color = "white") + scale_fill_gradient(low = "lightgreen", high = "red") + labs(x = "Variable", y = "Variable", title = "Correlation heatmap for features")
rm(r)

# First check point: outliers are removed. data are normalized.
save.image(file = "../../Datasets/Checkpoints/checkPoint_01.RData")
rm(list = ls())

# ALL THE FEATURES -------------------------
# In this part, all the features are used
load(file = "../../Datasets/Checkpoints/checkPoint_01.RData")

# How Many Clusters ====

# K-Means with different number of clusters
possible_clustering <- data.frame(matrix(ncol = 20, nrow = nrow(df)))
max.noClusters <- 50
wss <- rep(0, max.noClusters)
for (c in 1:max.noClusters) {
     kmeans.out <- kmeans(normal.df, centers = c, iter.max = 200)
     wss[c] <- kmeans.out$tot.withinss
     possible_clustering[, c] <- kmeans.out$cluster
}
rm(c, max.noClusters)

# Scree plot to see how many clusters are required.
g <- ggplot(data = data.frame(y = wss)) + geom_point(aes(x = seq_along(y), y = y), size = 5, fill = "black") + geom_line(aes(x = seq_along(y), y = y, group = "a"), color = "blue") + labs(y = "Sum of Squared Error (WSS)", x = "Number of Clusters")
g

# It becomes obvious that we should have k = 5 features.
number.of.clusters <- 5
g + geom_vline(xintercept = number.of.clusters, colour = "red")

rm(wss, kmeans.out, g)

# Plotting Segments ====

# Parallel coordinate plot - based on clusters
cluster.labels <- factor(possible_clustering[, number.of.clusters])
rm(possible_clustering)
parallel.plot(normal.df, target.df$accpid, cluster.labels)

# 2-D plots of segments
plot.2D.allFeatures(normal.df, cluster.labels)


# Checking the Target Variables =============================
# change the target_idx for more plots
targetVar <- factor(target.df$total_score_disq >= 2)

# Table of actual labels and clusters
table.actualLabel.clusterLabel(targetVar, cluster.labels)

# Parallel Coordinate Plot - based on target variables
parallel.plot(normal.df, target.df$accpid, targetVar)

# 2-D plots - based on target variables
plot.2D.allFeatures(normal.df, targetVar)

# Second check point: analysis for all the features are done.
save.image(file = "../../Datasets/Checkpoints/checkPoint_02.RData")
rm(list = ls())

# WALK-LIKE FEATURES ------------------------------------------------------------------
#Use the same analysis, but instead just include the walk-like features
# In this part, all the features are used
load(file = "../../Datasets/Checkpoints/checkPoint_01.RData")
normal.df <- normal.df[, c(1:5, 11)]

# How Many Clusters ====

# K-Means with different number of clusters
possible_clustering <- data.frame(matrix(ncol = 20, nrow = nrow(normal.df)))
max.noClusters <- 50
wss <- rep(0, max.noClusters)
for (c in 1:max.noClusters) {
     kmeans.out <- kmeans(normal.df, centers = c, iter.max = 200)
     wss[c] <- kmeans.out$tot.withinss
     possible_clustering[, c] <- kmeans.out$cluster
}
rm(c, max.noClusters)

# Scree plot to see how many clusters are required.
g <- ggplot(data = data.frame(y = wss)) + geom_point(aes(x = seq_along(y), y = y), size = 5, fill = "black") + geom_line(aes(x = seq_along(y), y = y, group = "a"), color = "blue") + labs(y = "Sum of Squared Error (WSS)", x = "Number of Clusters")
g

# It becomes obvious that we should have k = 5 features.
number.of.clusters <- 5
g + geom_vline(xintercept = number.of.clusters, colour = "red")

rm(wss, kmeans.out, g)

# Plotting Segments ====

# Parallel coordinate plot - based on clusters
cluster.labels <- factor(possible_clustering[, number.of.clusters])
rm(possible_clustering)
parallel.plot(normal.df, target.df$accpid, cluster.labels)

# 2-D plots of segments
plot.2D.WalkFeatures(normal.df, cluster.labels)


# Checking the Target Variables =============================
# change the target_idx for more plots
targetVar <- factor(target.df$sub_sppb)

# Table of actual labels and clusters
table.actualLabel.clusterLabel(targetVar, cluster.labels)

# Parallel Coordinate Plot - based on target variables
parallel.plot(normal.df, target.df$accpid, targetVar)

# 2-D plots - based on target variables
plot.2D.WalkFeatures(normal.df, targetVar)

# Third check point: analysis for just walk-like features are done.
save.image(file = "../../Datasets/Checkpoints/checkPoint_03.RData")
rm(list = ls())


# WALK-LIKES AND NONWALKS USED SEPARATELY ---------------------------------------------
# This is also named as Matrix clusterin?

load(file = "../../Datasets/Checkpoints/checkPoint_01.RData")
w.df <- normal.df[, c(1:5, 11)]
nw.df <- normal.df[, c(6:10)]
rm(normal.df)

# How Many Clusters for Walks ====

# K-Means with different number of clusters
possible_clustering.w <- data.frame(matrix(ncol = 20, nrow = nrow(w.df)))
max.noClusters <- 50
wss <- rep(0, max.noClusters)
for (c in 1:max.noClusters) {
     kmeans.out <- kmeans(w.df, centers = c, iter.max = 200)
     wss[c] <- kmeans.out$tot.withinss
     possible_clustering.w[, c] <- kmeans.out$cluster
}
rm(c, max.noClusters)

# Scree plot to see how many clusters are required.
g <- ggplot(data = data.frame(y = wss)) + geom_point(aes(x = seq_along(y), y = y), size = 5, fill = "black") + geom_line(aes(x = seq_along(y), y = y, group = "a"), color = "blue") + labs(y = "Sum of Squared Error (WSS)", x = "Number of Clusters (Walks)")
g

# It becomes obvious that we should have k = 4 features.
number.of.clusters.w <- 5
gw <- g + geom_vline(xintercept = number.of.clusters.w, colour = "red")

rm(wss, kmeans.out, g)


# How Many Clusters for Non-Walks ====

# K-Means with different number of clusters
possible_clustering.nw <- data.frame(matrix(ncol = 20, nrow = nrow(nw.df)))
max.noClusters <- 50
wss <- rep(0, max.noClusters)
for (c in 1:max.noClusters) {
     kmeans.out <- kmeans(nw.df, centers = c, iter.max = 200)
     wss[c] <- kmeans.out$tot.withinss
     possible_clustering.nw[, c] <- kmeans.out$cluster
}
rm(c, max.noClusters)

# Scree plot to see how many clusters are required.
g <- ggplot(data = data.frame(y = wss)) + geom_point(aes(x = seq_along(y), y = y), size = 5, fill = "black") + geom_line(aes(x = seq_along(y), y = y, group = "a"), color = "blue") + labs(y = "Sum of Squared Error (WSS)", x = "Number of Clusters (Non Walks)")
g

# It becomes obvious that we should have k = 5 features.
number.of.clusters.nw <- 4
gnw <- g + geom_vline(xintercept = number.of.clusters.nw, colour = "red")

rm(wss, kmeans.out, g)

grid.arrange(gw, gnw, nrow = 2)

rm(gw, gnw)

cluster.labels.w <- factor(possible_clustering.w[, number.of.clusters.w])
cluster.labels.nw <- factor(possible_clustering.nw[, number.of.clusters.nw])
rm(possible_clustering.nw, possible_clustering.w)

# Parallel coordinate plot - based on clusters
g.w <- parallel.plot(w.df, target.df$accpid, cluster.labels.w)
g.nw <- parallel.plot(nw.df, target.df$accpid, cluster.labels.nw)
grid.arrange(g.w, g.nw, nrow = 2)
rm(g.w, g.nw)

# Checking the Target Variables =============================
# change the target_idx for more plots
targetVar <- factor(target.df$sub_sppb)

plot.matrix.clusters(cluster.labels.w, cluster.labels.nw, targetVar)

temp.plot.df <- data.frame(feat1 = w.df$W1_prc, feat2 = nw.df$NW1_prc)
plot.2D.matrix.clusters(temp.plot.df, cluster.labels.w, cluster.labels.nw, targetVar)

rm(temp.plot.df)

# Fifth check point: analysis for matrix cluster are done.
save.image(file = "../../Datasets/Checkpoints/checkPoint_05.RData")
rm(list = ls())

# PCA FEATURES -----------------------------------
#Use the same analysis, but instead we use PCA features
# In this part, all the features are used
load(file = "../../Datasets/Checkpoints/checkPoint_01.RData")

pca.out <- prcomp(normal.df)
a <- data.frame(t(summary(pca.out)$importance))
g_pca <- ggplot(data = a, aes(x = seq_along(Cumulative.Proportion), y = Cumulative.Proportion))
g_pca + geom_bar(aes(fill = factor(c(rep(1, 3), 2, rep(1, 7)))), stat = "identity") + scale_fill_manual(values = c("black", "blue")) + guides(fill = F) + labs(x = "Principal Components") + scale_x_discrete(breaks = 1:11, limits = 1:11)
rm(g_pca, a)

number.of.pcs <- 4
weights <- data.frame((pca.out$rotation[, 1:number.of.pcs]) / (pca.out$sdev[1:number.of.pcs]))

weights$feature <- factor(rownames(weights), levels = rownames(weights))
weights <- melt(weights)
weights$Weight <- weights$value
weights$value <- NULL
ggplot(data = weights, aes(x = variable, y = feature)) + geom_tile(aes(fill = Weight), color = "white") + scale_fill_gradient(low = "green", high = "red")
rm(weights)


pca.df <- predict(pca.out, normal.df)[, 1:number.of.pcs]
pca.df <- data.frame(pca.df[, 1:number.of.pcs])
rm(pca.out)

# How Many Clusters ====

# K-Means with different number of clusters
possible_clustering <- data.frame(matrix(ncol = 20, nrow = nrow(pca.df)))
max.noClusters <- 50
wss <- rep(0, max.noClusters)
for (c in 1:max.noClusters) {
     kmeans.out <- kmeans(pca.df, centers = c, iter.max = 200)
     wss[c] <- kmeans.out$tot.withinss
     possible_clustering[, c] <- kmeans.out$cluster
}
rm(c, max.noClusters)

# Scree plot to see how many clusters are required.
g <- ggplot(data = data.frame(y = wss)) + geom_point(aes(x = seq_along(y), y = y), size = 5, fill = "black") + geom_line(aes(x = seq_along(y), y = y, group = "a"), color = "blue") + labs(y = "Sum of Squared Error (WSS)", x = "Number of Clusters")
g

# It becomes obvious that we should have k = 5 features.
number.of.clusters <- 5
g + geom_vline(xintercept = number.of.clusters, colour = "red")

rm(wss, kmeans.out, g)

# Plotting Segments ====

# Parallel coordinate plot - based on clusters
cluster.labels <- factor(possible_clustering[, number.of.clusters])
rm(possible_clustering)
parallel.plot(pca.df, target.df$accpid, cluster.labels)

# 2-D plots of segments
plot.2D.PCAFeatures(pca.df, cluster.labels)

# Checking the Target Variables =============================
# change the target_idx for more plots
targetVar <- factor(target.df$sub_sppb)

# Table of actual labels and clusters
table.actualLabel.clusterLabel(targetVar, cluster.labels)

# Parallel Coordinate Plot - based on target variables
parallel.plot(pca.df, target.df$accpid, targetVar)

# 2-D plots - based on target variables
plot.2D.PCAFeatures(pca.df, targetVar)

# Forth check point: analysis for PCA features are done.
save.image(file = "../../Datasets/Checkpoints/checkPoint_04.RData")
rm(list = ls())
