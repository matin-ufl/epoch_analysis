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
     na.idx <- which(is.na(df))
     result <- as.matrix(df)
     result[na.idx] <- 0L
     result <- data.frame(result)
     result[] <- mapply(FUN = as, result, sapply(df, class), SIMPLIFY = FALSE)
     result
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





