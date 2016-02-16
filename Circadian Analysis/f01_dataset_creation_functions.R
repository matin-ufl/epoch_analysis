# Required functions for Circadian analysis

# Clear function
clr <- function() {cat(rep("\n", 50))}

# Those files' information which have the minimum valid days (10+ wear times)
valid_participants <- function(PID_VC_HID, valid.days = 5) {
     selectedPIDs <- PID_VC_HID[PID_VC_HID$seq == 0, ]
     selectedPIDs <- selectedPIDs[selectedPIDs$valid_days >= valid.days, ]
     selectedPIDs
}


# Find wear times
find.wearTime <- function(accelerometer.1s.data = AC.1s) {
     mydata1m <- dataCollapser(accelerometer.1s.data, TS = "TimeStamp", col = "VM", by = 60)
     data1m = wearingMarking(dataset = mydata1m,
                             frame = 90, 
                             perMinuteCts = 1,
                             TS = "TimeStamp",
                             cts = "VM", 
                             streamFrame = NULL, 
                             allowanceFrame= 2, 
                             newcolname = "wearing")
     
     a <- sumVct(data1m, id="sdata1m")
}


# Transition-wise investigation.
# One of the MAIN functions to call from the script.
# Construct the features for every transition for a participant. Later that can be reduced to one row for participant.
# The main purpose to keep the output transition-wise is that to show the circadian rythm.
construct.features.at.transitions <- function(accelerometer.1s.data, wearTimes.info, threshold.minute = 600) {
     timeStamp <- sapply(as.character(accelerometer.1s.data$TimeStamp), function(timeStr) {as.character(unlist(strsplit(timeStr, " "))[[2]])})
     accelerometer.1s.data$time <- timeStamp
     
     transitions <- data.frame(period = factor(c("morning", "noon", "afternoon", "evening"), levels = c("morning", "noon", "afternoon", "evening")),
                               period.start = c("08:00:00", "11:00:00", "14:00:00", "17:00:00"),
                               period.end = c("11:00:00", "14:00:00", "17:00:00", "20:00:00"))
     
     result <- data.frame(matrix(nrow = 0, ncol = 11))
     colnames(result) <- c("a1.avg", "a1.std", "a1.energy", "a1.locomotion.avg", "a1.locomotion.energy",
                           "steps", "locomotion",
                           "vm.avg", "vm.std", "vm.energy",
                           "transition")
     
     for(day in 1:nrow(wearTimes.info)) {
          if(wearTimes.info$duration[day] >= threshold.minute) {
               day.start <- ((wearTimes.info$start[day] - 1) * 60) + 1
               day.end <- ((wearTimes.info$end[day] - 1) * 60) + 1
               for(transition in 1:nrow(transitions)) {
                    start.idx <- start.time.calculate(accelerometer.1s.data$time[day.start:day.end],
                                                      day.start,
                                                      as.character(transitions$period.start[transition]))
                    end.idx <- end.time.calculate(accelerometer.1s.data$time[day.start:day.end],
                                                  day.start,
                                                  as.character(transitions$period.end[transition]))
                    if(is.na(start.idx) && !is.na(end.idx)) {
                         start.idx <- day.start
                    } else if(!is.na(start.idx) && is.na(end.idx)) {
                         end.idx <- day.end
                    }
                    
                    if(!is.na(start.idx) && !is.na(end.idx)) {
                         selected.accelerometer.1s <- accelerometer.1s.data[start.idx:end.idx, ]
                         new.row <- features.from.accelerometer(selected.accelerometer.1s, 15, transitions$period[transition])
                         result <- data.frame(rbind(result, new.row))
                    }
               }
          }
     }
     result$PID <- rep(PID, nrow(result))
     result
}

# Finding the starting point for a transition in the selected wear time period.
start.time.calculate <- function(selected.timeStampStr, day.start, transition.startStr) {
     transition.start <- which(selected.timeStampStr == transition.startStr)
     if(length(transition.start) > 0) {
          return(day.start + transition.start)
     }
     return(NA)
}

# Finding the end point for a transition in the selected wear time period.
end.time.calculate <- function(selected.timeStampStr, day.start, transition.endStr) {
     transition.end <- which(selected.timeStampStr == transition.endStr)
     if(length(transition.end) > 0){
          return(day.start + transition.end)
     }
     return(NA)
}

# Constructs all the simple features (just Axis 1 and Vector Magnitude)
features.from.accelerometer <- function(accelerometer.data = selected.accelerometer.1s, epoch_length = 15, transition) {
     result.details <- data.frame(matrix(nrow = 0, ncol = 10))
     colnames(result.details) <- c("a1.avg", "a1.std", "a1.energy",
                           "steps", "locomotion",
                           "vm.avg", "vm.std", "vm.energy")
     for (sec in seq(1, nrow(accelerometer.data), by = epoch_length)) {
          start.idx <- sec
          end.idx <- min((sec + 15), nrow(accelerometer.data))
          # In our chunks at least 10 seconds should be present
          if((end.idx - start.idx) >= 10) {
               a1 <- smooth.acitivityPoints(accelerometer.data$axis1[start.idx:end.idx])
               VM <- smooth.acitivityPoints(accelerometer.data$VM[start.idx:end.idx])
               new.row <- data.frame(a1.avg = mean(a1), a1.std = sd(a1), a1.energy = sum(a1^2),
                                     steps = sum(accelerometer.data$steps[start.idx:end.idx]), locomotion = isLocomotion(selected.piece.ac = a1, nonZero.threshold = 0.6),
                                     vm.avg = mean(VM), vm.std = sd(VM), vm.energy = sum(VM^2))
               result.details <- data.frame(rbind(result.details, new.row))
          }
     }
     
     # Check this part to see what the final features in a transition
     result <- data.frame(a1.avg = mean(result.details$a1.avg),
                          a1.std = mean(result.details$a1.std),
                          a1.energy = mean(result.details$a1.energy),
                          a1.locomotion.avg = mean(result.details$a1.avg[result.details$locomotion]),
                          a1.locomotion.energy = mean(result.details$a1.energy[result.details$locomotion]),
                          steps = sum(result.details$steps),
                          locomotion = sum(result.details$locomotion == T),
                          vm.avg = mean(result.details$vm.avg),
                          vm.std = mean(result.details$vm.std),
                          vm.energy = mean(result.details$vm.energy),
                          transition = as.character(transition))
     result
}

# Rejects activity points which fall into P.95 or higher.
smooth.acitivityPoints <- function(selected.piece.ac, threshold = 0.95) {
     t <- quantile(selected.piece.ac, threshold, na.rm = T)[[1]]
     result <- selected.piece.ac
     if(t > 0) {
          temp <- selected.piece.ac[-(which(selected.piece.ac > t))]
          if(length(temp) > 0) {
               result <- selected.piece.ac[-(which(selected.piece.ac > t))]
          }
     }
     result
}

# If the non zero points are dominant and CV is low, then it is locomotion
isLocomotion <- function(selected.piece.ac = a1, nonZero.threshold = 0.70) {
     if(length(selected.piece.ac) > 0) {
          if((sum(selected.piece.ac > 0, na.rm = T) / length(selected.piece.ac)) > nonZero.threshold) {
               return(T)
          }
     }
     return(F)
     
}

# Aggregation of all transitions
aggregate.transitions <- function(features.at.transitions = result, transitions) {
     final.row <- data.frame(matrix(nrow = 1, ncol = 10 * nrow(transitions)))
     colnames(final.row) <- c("morning.a1.avg", "morning.a1.std", "morning.a1.energy", "morning.a1.locomotion.avg", "morning.a1.locomotion.energy", "morning.steps", "morning.locomotion", "morning.vm.avg", "morning.vm.std", "morning.vm.energy",
                              "noon.a1.avg", "noon.a1.std", "noon.a1.energy", "noon.a1.locomotion.avg", "noon.a1.locomotion.energy", "noon.steps", "noon.locomotion", "noon.vm.avg", "noon.vm.std", "noon.vm.energy",
                              "afternoon.a1.avg", "afternoon.a1.std", "afternoon.a1.energy", "afternoon.a1.locomotion.avg", "afternoon.a1.locomotion.energy", "afternoon.steps", "afternoon.locomotion", "afternoon.vm.avg", "afternoon.vm.std", "afternoon.vm.energy",
                              "evening.a1.avg", "evening.a1.std", "evening.a1.energy", "evening.a1.locomotion.avg", "evening.a1.locomotion.energy", "evening.steps", "evening.locomotion", "evening.vm.avg", "evening.vm.std", "evening.vm.energy")
     for (transition in transitions$period) {
          # Morning
          if(transition == transitions$period[1]) {
               morning.idx <- which(features.at.transitions$transition == as.character(transition))
               morning.features <- features.at.transitions[morning.idx, ]
               final.row$morning.a1.avg = mean(morning.features$a1.avg)
               final.row$morning.a1.std = mean(morning.features$a1.std)
               final.row$morning.a1.energy = mean(morning.features$a1.energy)
               final.row$morning.a1.locomotion.avg = mean(morning.features$a1.locomotion.avg)
               final.row$morning.a1.locomotion.energy = mean(morning.features$a1.locomotion.energy)
               final.row$morning.steps = mean(morning.features$steps)
               final.row$morning.locomotion = mean(morning.features$locomotion)
               final.row$morning.vm.avg = mean(morning.features$vm.avg)
               final.row$morning.vm.std = mean(morning.features$vm.std)
               final.row$morning.vm.energy = mean(morning.features$vm.energy)
               rm(morning.idx, morning.features)
          } else if(transition == transitions$period[2]) { # noon
               noon.idx <- which(features.at.transitions$transition == as.character(transition))
               noon.features <- features.at.transitions[noon.idx, ]
               final.row$noon.a1.avg = mean(noon.features$a1.avg)
               final.row$noon.a1.std = mean(noon.features$a1.std)
               final.row$noon.a1.energy = mean(noon.features$a1.energy)
               final.row$noon.a1.locomotion.avg = mean(noon.features$a1.locomotion.avg)
               final.row$noon.a1.locomotion.energy = mean(noon.features$a1.locomotion.energy)
               final.row$noon.steps = mean(noon.features$steps)
               final.row$noon.locomotion = mean(noon.features$locomotion)
               final.row$noon.vm.avg = mean(noon.features$vm.avg)
               final.row$noon.vm.std = mean(noon.features$vm.std)
               final.row$noon.vm.energy = mean(noon.features$vm.energy)
               rm(noon.idx, noon.features)
          } else if(transition == transitions$period[3]) { # afternoon
               afternoon.idx <- which(features.at.transitions$transition == as.character(transition))
               afternoon.features <- features.at.transitions[afternoon.idx, ]
               final.row$afternoon.a1.avg = mean(afternoon.features$a1.avg)
               final.row$afternoon.a1.std = mean(afternoon.features$a1.std)
               final.row$afternoon.a1.energy = mean(afternoon.features$a1.energy)
               final.row$afternoon.a1.locomotion.avg = mean(afternoon.features$a1.locomotion.avg)
               final.row$afternoon.a1.locomotion.energy = mean(afternoon.features$a1.locomotion.energy)
               final.row$afternoon.steps = mean(afternoon.features$steps)
               final.row$afternoon.locomotion = mean(afternoon.features$locomotion)
               final.row$afternoon.vm.avg = mean(afternoon.features$vm.avg)
               final.row$afternoon.vm.std = mean(afternoon.features$vm.std)
               final.row$afternoon.vm.energy = mean(afternoon.features$vm.energy)
               rm(afternoon.idx, afternoon.features)
          } else if(transition == transitions$period[4]) { # evening
               evening.idx <- which(features.at.transitions$transition == as.character(transition))
               evening.features <- features.at.transitions[evening.idx, ]
               final.row$evening.a1.avg = mean(evening.features$a1.avg)
               final.row$evening.a1.std = mean(evening.features$a1.std)
               final.row$evening.a1.energy = mean(evening.features$a1.energy)
               final.row$evening.a1.locomotion.avg = mean(evening.features$a1.locomotion.avg)
               final.row$evening.a1.locomotion.energy = mean(evening.features$a1.locomotion.energy)
               final.row$evening.steps = mean(evening.features$steps)
               final.row$evening.locomotion = mean(evening.features$locomotion)
               final.row$evening.vm.avg = mean(evening.features$vm.avg)
               final.row$evening.vm.std = mean(evening.features$vm.std)
               final.row$evening.vm.energy = mean(evening.features$vm.energy)
               rm(evening.idx, evening.features)
          }
          final.row
     }
}

