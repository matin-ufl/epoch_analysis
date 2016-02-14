# This script is written for visualizing the created dataset by circadian package
library(ggplot2)
library(gridExtra)

setwd("~/Dropbox/Work-Research/Current Directory/Bag of Words/epoch_analysis/")

participant.df <- read.csv("../Datasets/Outputs - Circadian Analysis/d01_participants_transitions_021216.csv")
participant.df[is.na(participant.df)] <- 0
participant.df$PID <- factor(participant.df$PID)
participant.df$transition <- factor(participant.df$transition, levels = c("morning", "noon", "afternoon", "evening"))

# Axis 1 Activity Count Average -------------------------
g1 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[1], ], aes(x = seq_along(a1.avg), y = a1.avg)) +
     geom_point(colour = "blue", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "", title = "Axis 1 Acticity Count (Average)")
g2 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[2], ], aes(x = seq_along(a1.avg), y = a1.avg)) +
     geom_point(colour = "red", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "")
g3 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[3], ], aes(x = seq_along(a1.avg), y = a1.avg)) +
     geom_point(colour = "green", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "")
g4 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[4], ], aes(x = seq_along(a1.avg), y = a1.avg)) +
     geom_point(colour = "purple", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "")

grid.arrange(g1, g2, g3, g4, nrow = 4)


# Locomotion -------------------------
g1 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[1], ], aes(x = seq_along(a1.avg), y = locomotion)) +
     geom_point(colour = "blue", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "", title = "Number of 15-sec Locomotions")
g2 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[2], ], aes(x = seq_along(a1.avg), y = locomotion)) +
     geom_point(colour = "red", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "")
g3 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[3], ], aes(x = seq_along(a1.avg), y = locomotion)) +
     geom_point(colour = "green", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "")
g4 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[4], ], aes(x = seq_along(a1.avg), y = locomotion)) +
     geom_point(colour = "purple", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "")

grid.arrange(g1, g2, g3, g4, nrow = 4)



# Energy Axis 1 -------------------------
g1 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[1], ], aes(x = seq_along(a1.avg), y = a1.energy)) +
     geom_point(colour = "blue", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "", title = "Axis 1 Energy (Average)")
g2 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[2], ], aes(x = seq_along(a1.avg), y = a1.energy)) +
     geom_point(colour = "red", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "")
g3 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[3], ], aes(x = seq_along(a1.avg), y = a1.energy)) +
     geom_point(colour = "green", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "")
g4 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[4], ], aes(x = seq_along(a1.avg), y = a1.energy)) +
     geom_point(colour = "purple", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "")

grid.arrange(g1, g2, g3, g4, nrow = 4)


# Energy During Locomotions -------------------------
g1 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[1], ], aes(x = seq_along(a1.avg), y = a1.locomotion.energy)) +
     geom_point(colour = "blue", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "", title = "Locomotions Energy (Average)")
g2 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[2], ], aes(x = seq_along(a1.avg), y = a1.locomotion.energy)) +
     geom_point(colour = "red", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "")
g3 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[3], ], aes(x = seq_along(a1.avg), y = a1.locomotion.energy)) +
     geom_point(colour = "green", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "")
g4 <- ggplot(data = participant.df[participant.df$PID == levels(participant.df$PID)[4], ], aes(x = seq_along(a1.avg), y = a1.locomotion.energy)) +
     geom_point(colour = "purple", size = 5) + scale_x_discrete(labels = participant.df$transition) + 
     theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(x = "", y = "")

grid.arrange(g1, g2, g3, g4, nrow = 4)

