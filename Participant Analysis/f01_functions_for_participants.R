color.code <- c("red", "blue", "green", "purple", "yellow", "black")

parallel.plot <- function(df, pid, targetVariable) {
     temp_df <- data.frame(melt(df), participant = rep(pid, ncol(df)), target = rep(targetVariable, ncol(df)))
     parallelCoordinatePlot <- ggplot(data = temp_df, aes(x = variable, y = value))
     parallelCoordinatePlot <- parallelCoordinatePlot + geom_line(aes(color = target, group = participant))
     if(is.factor(targetVariable)) {
          parallelCoordinatePlot + scale_color_manual(values = color.code)
     } else {
          parallelCoordinatePlot + scale_color_continuous(low = "yellow", high = "darkred")
     }
}

plot.2D.allFeatures <- function(dataFrame = normal.df, selected.labels = cluster.labels) {
     general_plot <- ggplot(data = data.frame(dataFrame, labs = selected.labels))
     g1 <- general_plot + geom_point(aes(x = NW4_prc, y = NW5_prc, colour = labs), size = 4) + scale_colour_manual(values = color.code)
     g2 <- general_plot + geom_point(aes(x = W4_prc, y = NW2_prc, colour = labs), size = 4) + scale_colour_manual(values = color.code)
     g3 <- general_plot + geom_point(aes(x = NW1_prc, y = W2_prc, colour = labs), size = 4) + scale_colour_manual(values = color.code)
     g4 <- general_plot + geom_point(aes(x = W5_prc, y = W2_prc, colour = labs), size = 4) + scale_colour_manual(values = color.code)
     grid.arrange(g1, g2, g3, g4, nrow = 2, ncol = 2)
}

plot.2D.WalkFeatures <- function(dataFrame = normal.df, selected.labels = cluster.labels) {
     general_plot <- ggplot(data = data.frame(dataFrame, labs = selected.labels))
     g1 <- general_plot + geom_point(aes(x = W1_prc, y = a1.avg_ac, colour = labs), size = 4) + scale_colour_manual(values = color.code)
     g2 <- general_plot + geom_point(aes(x = W4_prc, y = W2_prc, colour = labs), size = 4) + scale_colour_manual(values = color.code)
     g3 <- general_plot + geom_point(aes(x = W3_prc, y = W5_prc, colour = labs), size = 4) + scale_colour_manual(values = color.code)
     grid.arrange(g1, g2, g3, nrow = 3, ncol = 1)
}

plot.2D.PCAFeatures <- function(dataFrame = pca.df, selected.labels = cluster.labels) {
     general_plot <- ggplot(data = data.frame(dataFrame, labs = selected.labels))
     g1 <- general_plot + geom_point(aes(x = PC1, y = PC2, colour = labs), size = 4) + scale_colour_manual(values = color.code)
     g2 <- general_plot + geom_point(aes(x = PC3, y = PC4, colour = labs), size = 4) + scale_colour_manual(values = color.code)
     grid.arrange(g1, g2, nrow = 2, ncol = 1)
}

table.actualLabel.clusterLabel <- function(actualLabel, clusterLabel) {
     table(actualLabel, clusterLabel)     
}


exclude.outlier <- function(data, threshold = floor(ncol(data)/5)) {
     # No identifier should be included in data
     counts <- rep(0, nrow(data))
     for (i in 1:ncol(data)) {
          colValues <- data[, i]
          tooBigIdx <- which(colValues > (sd(colValues) * 5))
          counts[tooBigIdx] <- counts[tooBigIdx] + 1
     }
     outlierIdx <- which(counts >= threshold)
     print(paste("Number of outliers excluded:", length(outlierIdx)))
     list(data = data[-outlierIdx, ], outlierIdx = outlierIdx)
}