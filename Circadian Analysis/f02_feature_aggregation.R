# Functions for epoch aggregation to participant's features

clr <- function() {cat(rep("\n", 50))}

simple_aggregate_one_participant.15sec <- function(oneParticipant.df) {
     result <- data.frame(PID = oneParticipant.df$PID[1],
                          morning.a1.avg = NA, morning.a1.std = NA, morning.a1.energy = NA, morning.a1.locomotion.avg = NA, morning.a1.locomotion.energy = NA, morning.steps = NA, morning.locomotion = NA, morning.vm.avg = NA, morning.vm.std = NA, morning.vm.energy = NA,
                          noon.a1.avg = NA, noon.a1.std = NA, noon.a1.energy = NA, noon.a1.locomotion.avg = NA, noon.a1.locomotion.energy = NA, noon.steps = NA, noon.locomotion = NA, noon.vm.avg = NA, noon.vm.std = NA, noon.vm.energy = NA,
                          afternoon.a1.avg = NA, afternoon.a1.std = NA, afternoon.a1.energy = NA, afternoon.a1.locomotion.avg = NA, afternoon.a1.locomotion.energy = NA, afternoon.steps = NA, afternoon.locomotion = NA, afternoon.vm.avg = NA, afternoon.vm.std = NA, afternoon.vm.energy = NA,
                          evening.a1.avg = NA, evening.a1.std = NA, evening.a1.energy = NA, evening.a1.locomotion.avg = NA, evening.a1.locomotion.energy = NA, evening.steps = NA, evening.locomotion = NA, evening.vm.avg = NA, evening.vm.std = NA, evening.vm.energy = NA)
     # Morning
     transition.df <- oneParticipant.df[oneParticipant.df$transition == "morning", ]
     result$morning.a1.avg <- mean(transition.df$a1.avg, na.rm = T)
     result$morning.a1.std <- mean(transition.df$a1.std, na.rm = T)
     result$morning.a1.energy <- mean(transition.df$a1.energy, na.rm = T)
     result$morning.a1.locomotion.avg <- mean(transition.df$a1.locomotion.avg, na.rm = T)
     result$morning.a1.locomotion.energy <- mean(transition.df$a1.locomotion.energy, na.rm = T)
     result$morning.steps <- mean(transition.df$steps, na.rm = T)
     result$morning.locomotion <- mean(transition.df$locomotion, na.rm = T)
     result$morning.vm.avg <- mean(transition.df$vm.avg, na.rm = T)
     result$morning.vm.std <- mean(transition.df$vm.std, na.rm = T)
     result$morning.vm.energy <- mean(transition.df$vm.energy, na.rm = T)
     
     # Noon
     transition.df <- oneParticipant.df[oneParticipant.df$transition == "noon", ]
     result$noon.a1.avg <- mean(transition.df$a1.avg, na.rm = T)
     result$noon.a1.std <- mean(transition.df$a1.std, na.rm = T)
     result$noon.a1.energy <- mean(transition.df$a1.energy, na.rm = T)
     result$noon.a1.locomotion.avg <- mean(transition.df$a1.locomotion.avg, na.rm = T)
     result$noon.a1.locomotion.energy <- mean(transition.df$a1.locomotion.energy, na.rm = T)
     result$noon.steps <- mean(transition.df$steps, na.rm = T)
     result$noon.locomotion <- mean(transition.df$locomotion, na.rm = T)
     result$noon.vm.avg <- mean(transition.df$vm.avg, na.rm = T)
     result$noon.vm.std <- mean(transition.df$vm.std, na.rm = T)
     result$noon.vm.energy <- mean(transition.df$vm.energy, na.rm = T)
     
     # Afternoon
     transition.df <- oneParticipant.df[oneParticipant.df$transition == "afternoon", ]
     result$afternoon.a1.avg <- mean(transition.df$a1.avg, na.rm = T)
     result$afternoon.a1.std <- mean(transition.df$a1.std, na.rm = T)
     result$afternoon.a1.energy <- mean(transition.df$a1.energy, na.rm = T)
     result$afternoon.a1.locomotion.avg <- mean(transition.df$a1.locomotion.avg, na.rm = T)
     result$afternoon.a1.locomotion.energy <- mean(transition.df$a1.locomotion.energy, na.rm = T)
     result$afternoon.steps <- mean(transition.df$steps, na.rm = T)
     result$afternoon.locomotion <- mean(transition.df$locomotion, na.rm = T)
     result$afternoon.vm.avg <- mean(transition.df$vm.avg, na.rm = T)
     result$afternoon.vm.std <- mean(transition.df$vm.std, na.rm = T)
     result$afternoon.vm.energy <- mean(transition.df$vm.energy, na.rm = T)

     # Evening     
     transition.df <- oneParticipant.df[oneParticipant.df$transition == "evening", ]
     result$evening.a1.avg <- mean(transition.df$a1.avg, na.rm = T)
     result$evening.a1.std <- mean(transition.df$a1.std, na.rm = T)
     result$evening.a1.energy <- mean(transition.df$a1.energy, na.rm = T)
     result$evening.a1.locomotion.avg <- mean(transition.df$a1.locomotion.avg, na.rm = T)
     result$evening.a1.locomotion.energy <- mean(transition.df$a1.locomotion.energy, na.rm = T)
     result$evening.steps <- mean(transition.df$steps, na.rm = T)
     result$evening.locomotion <- mean(transition.df$locomotion, na.rm = T)
     result$evening.vm.avg <- mean(transition.df$vm.avg, na.rm = T)
     result$evening.vm.std <- mean(transition.df$vm.std, na.rm = T)
     result$evening.vm.energy <- mean(transition.df$vm.energy, na.rm = T)
     
     result
}

replace_NA_with_zero <- function(df) {
     for (i in 1:ncol(df)) {
          nan.idx <- which(is.na(df[, i]))
          if(length(nan.idx)) {
               df[nan.idx, i] <- 0
          }
     }
     df
}

outlier.detection <- function(dataFrame = df, exclude.column = 1, count.threshold = floor(ncol(df)/5), value.threshold = 2) {
     dt <- dataFrame[, -exclude.column]
     counts <- rep(0, nrow(dataFrame))
     for (i in 1:nrow(dt)) {
          for(j in 1:ncol(dt)) {
               if(abs(dt[i, j] - mean(dt[, j])) > value.threshold * sd(dt[, j])) {
                    counts[i] <- counts[i] + 1
               }
          }
     }
     which(counts > count.threshold)
}

ordered.targetVariable <- function(PIDs, target.PIDs, targetVar) {
     result <- data.frame(PID = PIDs, targetVar = rep(NA, length(PIDs)))
     for(i in 1:length(PIDs)) {
          temp <- targetVar[which(target.PIDs == PIDs[i])]
          if(length(temp) > 0) {
               result$targetVar[i] <- temp
          }
     }
     result
}

parallel.plot <- function(dataFrame = df, targetVar) {
     targetVar <- factor(targetVar)
     data.plot <- melt(data.frame(dataFrame[, -1], class = targetVar, id = factor(1:nrow(dataFrame))))
     g <- ggplot(data = data.plot)
     g <- g + geom_line(aes(x = variable, y = value, group = id, colour = class))
     g <- g + scale_colour_manual(values = color.code)
     g + theme_bw()
}


label.on.value <- function(epoch.df, feature.idx) {
     epoch.label <- factor(c("most.inactive", "inactive", "active", "most.active"), levels = c("most.inactive", "inactive", "active", "most.active"))
     result <- data.frame(word = rep(NA, nrow(epoch.df)))
     q.morning <- quantile(epoch.df[epoch.df$transition == "morning", feature.idx])
     q.noon <- quantile(epoch.df[epoch.df$transition == "noon", feature.idx])
     q.afternoon <- quantile(epoch.df[epoch.df$transition == "afternoon", feature.idx])
     q.evening <- quantile(epoch.df[epoch.df$transition == "evening", feature.idx])
     for (i in 1:nrow(epoch.df)) {
          print(i)
          if(epoch.df$transition[i] == "morning") {
               result$word[i] <- private.giveMeLabel(epoch.df[i, feature.idx], q.morning, epoch.label)
          } else if(epoch.df$transition[i] == "noon") {
               result$word[i] <- private.giveMeLabel(epoch.df[i, feature.idx], q.noon, epoch.label)
          } else if(epoch.df$transition[i] == "afternoon") {
               result$word[i] <- private.giveMeLabel(epoch.df[i, feature.idx], q.afternoon, epoch.label)
          } else {
               result$word[i] <- private.giveMeLabel(epoch.df[i, feature.idx], q.evening, epoch.label)
          }
     }
     result
}

private.giveMeLabel <- function(value, q, labels) {
     if(value < q[2]) {
          return(levels(labels)[1])
     } else if(value < q[3]) {
          return(levels(labels)[2])
     } else if(value < q[4]) {
          return(levels(labels)[3])
     } else {
          return(levels(labels)[4])
     }
}

label.aggregate.oneParticipant <- function(oneParticipant.label.df) {
     result <- data.frame(PID = oneParticipant.label.df$PID[1],
                          morning.mostInactive = 0, morning.inactive = 0, morning.active = 0, morning.mostActive = 0,
                          noon.mostInactive = 0, noon.inactive = 0, noon.active = 0, noon.mostActive = 0,
                          afternoon.mostInactive = 0, afternoon.inactive = 0, afternoon.active = 0, afternoon.mostActive = 0,
                          evening.mostInactive = 0, evening.inactive = 0, evening.active = 0, evening.mostActive = 0)
     
     # Morning
     day.time <- oneParticipant.label.df[oneParticipant.label.df$time.of.day == "morning", ]
     result$morning.mostInactive <- length(which(day.time$word == "most.inactive")) / length(day.time$word)
     result$morning.inactive <- length(which(day.time$word == "inactive")) / length(day.time$word)
     result$morning.active <- length(which(day.time$word == "active")) / length(day.time$word)
     result$morning.mostActive <- length(which(day.time$word == "most.active")) / length(day.time$word)
     
     # Noon
     day.time <- oneParticipant.label.df[oneParticipant.label.df$time.of.day == "noon", ]
     result$noon.mostInactive <- length(which(day.time$word == "most.inactive")) / length(day.time$word)
     result$noon.inactive <- length(which(day.time$word == "inactive")) / length(day.time$word)
     result$noon.active <- length(which(day.time$word == "active")) / length(day.time$word)
     result$noon.mostActive <- length(which(day.time$word == "most.active")) / length(day.time$word)
     
     # Afternoon
     day.time <- oneParticipant.label.df[oneParticipant.label.df$time.of.day == "afternoon", ]
     result$afternoon.mostInactive <- length(which(day.time$word == "most.inactive")) / length(day.time$word)
     result$afternoon.inactive <- length(which(day.time$word == "inactive")) / length(day.time$word)
     result$afternoon.active <- length(which(day.time$word == "active")) / length(day.time$word)
     result$afternoon.mostActive <- length(which(day.time$word == "most.active")) / length(day.time$word)
     
     # Evening
     day.time <- oneParticipant.label.df[oneParticipant.label.df$time.of.day == "evening", ]
     result$evening.mostInactive <- length(which(day.time$word == "most.inactive")) / length(day.time$word)
     result$evening.inactive <- length(which(day.time$word == "inactive")) / length(day.time$word)
     result$evening.active <- length(which(day.time$word == "active")) / length(day.time$word)
     result$evening.mostActive <- length(which(day.time$word == "most.active")) / length(day.time$word)
     
     result
}
