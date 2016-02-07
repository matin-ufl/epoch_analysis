library(scales)
library(reshape2)
library(ggplot2)
library(gridExtra)

# Loading dataset####
setwd("~/Dropbox/Work-Research/Current Directory/Bag of Words/R Scripts/02 First Version Continue/Participant Analysis/")

load(file = "../../../Datasets/Epoch Dataset_Jan2016/03_participants_plus_a1AvgAc_baseline_011616.RData")
df <- participant_df[, c(1:7, 13:17, 23)]
rm(participant_df)

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
rm(outlier, outlierIdx)

normal.df <- data.frame(df[, c(1:2)], scale(df[, 3:13]))

# Checking correlation between features####
r <- data.frame(cor(normal.df[, 3:13]))
r$feature <- factor(rownames(r), levels = rownames(r))
r <- melt(r)
r <- ddply(r, .(variable), transform)
ggplot(data = r, aes(x = variable, y = feature)) + geom_tile(aes(fill = abs(value)), color = "white") + scale_fill_gradient(low = "lightgreen", high = "red")
rm(r)

# How many clusters####
possible_clustering <- data.frame(matrix(ncol = 20, nrow = nrow(df)))
max.noClusters <- 50
wss <- rep(0, max.noClusters)
for (c in 1:max.noClusters) {
     kmeans.out <- kmeans(normal.df[, 3:13], centers = c, iter.max = 200)
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


# Parallel Coordinate Plot - based on clusters####
cluster.labels <- factor(possible_clustering[, number.of.clusters])
rm(possible_clustering)
cluster.colours <- c("yellow", "blue", "green", "purple", "red", "black")[1:number.of.clusters]
temp_df <- data.frame(melt(normal.df[, 3:13]), participant = rep(normal.df$pid, (ncol(normal.df) - 2)), cluster = rep(cluster.labels, (ncol(normal.df) - 2)))
parallelCoordinatePlot <- ggplot(data = temp_df, aes(x = variable, y = value))
parallelCoordinatePlot <- parallelCoordinatePlot + geom_line(aes(colour = factor(cluster), group = participant))
parallelCoordinatePlot + scale_colour_manual(values = cluster.colours)

rm(parallelCoordinatePlot, temp_df, number.of.clusters)

general_plot <- ggplot(data = normal.df)
g1 <- general_plot + geom_point(aes(x = NW4_prc, y = NW5_prc, colour = cluster.labels), size = 4) + scale_colour_manual(values = cluster.colours)
g2 <- general_plot + geom_point(aes(x = W4_prc, y = NW2_prc, colour = cluster.labels), size = 4) + scale_colour_manual(values = cluster.colours)
g3 <- general_plot + geom_point(aes(x = NW1_prc, y = W2_prc, colour = cluster.labels), size = 4) + scale_colour_manual(values = cluster.colours)
g4 <- general_plot + geom_point(aes(x = W5_prc, y = W2_prc, colour = cluster.labels), size = 4) + scale_colour_manual(values = cluster.colours)
a <- grid.arrange(g1, g2, g3, g4, nrow = 2, ncol = 2)
rm(general_plot, g1, g2, g3, g4, a)

# Parallel Coordinate Plot - based on walking speed####
temp_df <- data.frame(melt(df[, 3:13]), participant = rep(df$pid, (ncol(df) - 2)), walkspeed = rep(df$walkspeed, (ncol(df) - 2)))
parallelCoordinatePlot <- ggplot(data = temp_df, aes(x = variable, y = value))
parallelCoordinatePlot <- parallelCoordinatePlot + geom_line(aes(color = walkspeed < 0.8, group = participant))
parallelCoordinatePlot + scale_color_manual(values = c("blue", "red"))


# Applying PCA to reduce unnecessary dimensionality####
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
