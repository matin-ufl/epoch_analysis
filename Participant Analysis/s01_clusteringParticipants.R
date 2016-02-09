library(ggplot2)
library(gridExtra)

# Loading dataset####
setwd("~/Dropbox/Work-Research/Current Directory/Bag of Words/R Scripts/02 First Version Continue/Participant Analysis/")

load(file = "../../../Datasets/Epoch Dataset_Jan2016/03_participants_plus_a1AvgAc_baseline_020816.RData")
df <- participant_df
rm(participant_df)

load(file = "../../../Datasets/Target Variables - Key Features/out01_baseline_selected_targetVariables_020716.RData")
target.df <- keyFeatures.df; rm(keyFeatures.df)
df <- df[df$pid %in% target.df$accpid, ]
target.df <- target.df[target.df$accpid %in% df$pid, ]

df <- df[with(data = df, order(pid)), ]
target.df <- target.df[with(data = target.df, order(accpid)), ]
target.df$walkspeed <- df$walkspeed
df$pid <- NULL; df$walkspeed <- NULL
# target.df contains all meta data and target variables
# df just contains the variables

outlier <- function(data, threshold = floor(ncol(data)/5)) {
     counts <- rep(0, nrow(data))
     for (i in 1:ncol(data)) {
          colValues <- data[, i]
          tooBigIdx <- which(colValues > (sd(colValues) * 5))
          counts[tooBigIdx] <- counts[tooBigIdx] + 1
     }
     which(counts >= threshold)
}

outlierIdx <- outlier(df)
print(paste("Number of outliers excluded:", length(outlierIdx)))

df <- df[-outlierIdx, ]
target.df <- target.df[-outlierIdx, ]
rm(outlier, outlierIdx)

normal.df <- data.frame(scale(df))

# Checking correlation between features####
r <- data.frame(cor(normal.df))
r$feature <- factor(rownames(r), levels = rownames(r))
r <- melt(r)
r <- ddply(r, .(variable), transform)
ggplot(data = r, aes(x = variable, y = feature)) + geom_tile(aes(fill = abs(value)), color = "white") + scale_fill_gradient(low = "lightgreen", high = "red") + labs(x = "Variable", y = "Variable", title = "Correlation heatmap for features")
rm(r)

# ALL THE FEATURES -------------------------
# In this part, all the features are used

# How Many Clusters ====

# K-Means with different number of clusters ####
possible_clustering <- data.frame(matrix(ncol = 20, nrow = nrow(df)))
max.noClusters <- 50
wss <- rep(0, max.noClusters)
for (c in 1:max.noClusters) {
     kmeans.out <- kmeans(normal.df, centers = c, iter.max = 200)
     wss[c] <- kmeans.out$tot.withinss
     possible_clustering[, c] <- kmeans.out$cluster
}
rm(c, max.noClusters)

g <- ggplot(data = data.frame(y = wss)) + geom_point(aes(x = seq_along(y), y = y), size = 5, fill = "black") + geom_line(aes(x = seq_along(y), y = y, group = "a"), color = "blue") + labs(y = "Sum of Squared Error (WSS)", x = "Number of Clusters")
g
number.of.clusters <- 5
g + geom_vline(xintercept = number.of.clusters, colour = "red")
# It becomes obvious that we should have k = 5 features.
rm(wss, kmeans.out, g)

# Plotting Segments ====

# Parallel coordinate plot - based on clusters ####
cluster.labels <- factor(possible_clustering[, number.of.clusters])
rm(possible_clustering)
cluster.colours <- c("yellow", "blue", "green", "purple", "red", "black")[1:number.of.clusters]
temp_df <- data.frame(melt(normal.df), participant = rep(target.df$accpid, ncol(normal.df)), cluster = rep(cluster.labels, ncol(normal.df)))
parallelCoordinatePlot <- ggplot(data = temp_df, aes(x = variable, y = value))
parallelCoordinatePlot <- parallelCoordinatePlot + geom_line(aes(colour = factor(cluster), group = participant))
parallelCoordinatePlot + scale_colour_manual(values = cluster.colours)

rm(parallelCoordinatePlot, temp_df, number.of.clusters)

# 2-D plots of segments ####
general_plot <- ggplot(data = normal.df)
g1 <- general_plot + geom_point(aes(x = NW4_prc, y = NW5_prc, colour = cluster.labels), size = 4) + scale_colour_manual(values = cluster.colours)
g2 <- general_plot + geom_point(aes(x = W4_prc, y = NW2_prc, colour = cluster.labels), size = 4) + scale_colour_manual(values = cluster.colours)
g3 <- general_plot + geom_point(aes(x = NW1_prc, y = W2_prc, colour = cluster.labels), size = 4) + scale_colour_manual(values = cluster.colours)
g4 <- general_plot + geom_point(aes(x = W5_prc, y = W2_prc, colour = cluster.labels), size = 4) + scale_colour_manual(values = cluster.colours)
a <- grid.arrange(g1, g2, g3, g4, nrow = 2, ncol = 2)
rm(general_plot, g1, g2, g3, g4, a)


# Checking the Target Variables =============================
# change the target_idx for more plots
source("f01_functions_for_participants.R")
targetVar <- factor(target.df$total_score_disq > 2)

# Table of actual labels and clusters ####
table.actualLabel.clusterLabel(targetVar, cluster.labels)

# Parallel Coordinate Plot - based on target variables####
parallel.plot(normal.df, target.df$accpid, targetVar)

rm(parallelCoordinatePlot, table.actualLabel.clusterLabel())


This part is not yet completed


# WALK-LIKE FEATURES ------------------------------------------------------------------
Use the same analysis, but instead just include the walk-like features


# WALK-LIKES AND NONWALKS USED SEPARATELY ---------------------------------------------
Consider clustering once based on walk-likes and once based on non-walks. Then construct a matrix and more



# Applying PCA to reduce unnecessary dimensionality -----------------------------------
pca.out <- prcomp(normal.df[, 3:13])
a <- data.frame(t(summary(pca.out)$importance))
g_pca <- ggplot(data = a, aes(x = seq_along(Cumulative.Proportion), y = Cumulative.Proportion))
g_pca + geom_bar(aes(fill = factor(c(rep(1, 4), 2, rep(1, 6)))), stat = "identity") + scale_fill_manual(values = c("black", "blue")) + guides(fill = F) + labs(x = "Principal Components") + scale_x_discrete(breaks = 1:11, limits = 1:11)
rm(g_pca, a)

number.of.pcs <- 5
weights <- data.frame((pca.out$rotation[, 1:number.of.pcs]) / (pca.out$sdev[1:number.of.pcs]))

weights$feature <- factor(rownames(weights), levels = rownames(weights))
weights <- melt(weights)
weights$Weight <- weights$value
weights$value <- NULL
ggplot(data = weights, aes(x = variable, y = feature)) + geom_tile(aes(fill = Weight), color = "white") + scale_fill_gradient(low = "green", high = "red")
rm(weights)


pc_df <- predict(pca.out, normal.df[, 3:13])[, 1:number.of.pcs]
pc_df <- data.frame(pid = df$pid, walkspeed = normal.df$walkspeed, pc_df)

# Clustering PCA dataset ####
possible_clustering <- data.frame(matrix(ncol = 20, nrow = nrow(df)))
max.noClusters <- 50
wss <- rep(0, max.noClusters)
for (c in 1:max.noClusters) {
     kmeans.out <- kmeans(pc_df[, 3:7], centers = c, iter.max = 200)
     wss[c] <- kmeans.out$tot.withinss
     possible_clustering[, c] <- kmeans.out$cluster
}
rm(c, max.noClusters)

number.of.clusters <- 5
g <- ggplot(data = data.frame(y = wss)) + geom_point(aes(x = seq_along(y), y = y), size = 5, fill = "black") + geom_line(aes(x = seq_along(y), y = y, group = "a"), color = "blue") + labs(y = "Sum of Squared Error (WSS)", x = "Number of Clusters")
g
g + geom_vline(xintercept = number.of.clusters, colour = "red")
# It becomes obvious that we should have k = 5 features.
rm(wss, kmeans.out, g)

# Parallel Coordinate Plot - PCA dataset####
cluster.labels <- factor(possible_clustering[, number.of.clusters])
rm(possible_clustering)
cluster.colours <- c("yellow", "blue", "green", "purple", "red", "black")[1:number.of.clusters]
temp_df <- data.frame(melt(pc_df[, 3:7]), participant = rep(pc_df$pid, (ncol(pc_df) - 2)), cluster = rep(cluster.labels, (ncol(pc_df) - 2)))
parallelCoordinatePlot <- ggplot(data = temp_df, aes(x = variable, y = value))
parallelCoordinatePlot <- parallelCoordinatePlot + geom_line(aes(colour = factor(cluster), group = participant))
parallelCoordinatePlot + scale_colour_manual(values = cluster.colours)

rm(parallelCoordinatePlot, temp_df, number.of.clusters)

general_plot <- ggplot(data = pc_df)
g1 <- general_plot + geom_point(aes(x = PC1, y = PC2, colour = cluster.labels), size = 4) + scale_colour_manual(values = cluster.colours)
g2 <- general_plot + geom_point(aes(x = PC3, y = PC5, colour = cluster.labels), size = 4) + scale_colour_manual(values = cluster.colours)
a <- grid.arrange(g1, g2, nrow = 2)
rm(general_plot, g1, g2, a)

g <- ggplot(data = pc_df[, 2:4], aes(x = PC1, y = PC2))
g <- g + geom_point(aes(color = walkspeed < 0.8, size = walkspeed < 0.8)) + scale_size_manual(values = c(4, 3)) + scale_color_manual(values = c("black", "green"))
g

