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
     mydata1m <- dataCollapser(accelerometer.1s.data, TS = "TimeStamp", col = "axis1", by = 60)
     data1m = wearingMarking(dataset = mydata1m,
                             frame = 90, 
                             perMinuteCts = 1,
                             TS = "TimeStamp",
                             cts = "axis1", 
                             streamFrame = NULL, 
                             allowanceFrame= 2, 
                             newcolname = "wearing")
     
     a <- sumVct(data1m, id="sdata1m")
}

# Main Function: construct epoch dataset
main.constructEpochDataset <- function(accelerometer.data = AC.1s, wearTimes.info, PID) {
     # I think this would reject obvious spikes!
     accelerometer.data$axis1 <- private.smoothAxis(accelerometer.data$axis1, threshold = 0.95)
     
     result <- data.frame(matrix(nrow = 0, ncol = 8))
     colnames(result) <- c("PID", "Day", "times.of.day",
                           "a1.avg", "a1.std", "a1.cv", "a1.energy", "steps")
     
     for(day in 1:nrow(wearTimes.info)) {
          day.info <- wearTimes.info[day, ]
          if(day.info$duration >= 600) {
               result <- data.frame(rbind(result, private.epochsOfAValidDay(accelerometer.data, day.info, day, PID)))
          }
     }
     result
}

private.epochsOfAValidDay <- function(accelerometer.data, day.info, day, PID) {
     result <- data.frame(matrix(nrow = 0, ncol = 8))
     colnames(result) <- c("PID", "Day", "times.of.day",
                          "a1.avg", "a1.std", "a1.cv", "a1.energy", "steps")
     day.start <- ((day.info$start - 1) * 60) + 1
     day.end <- ((day.info$end - 1) * 60) + 1
     for (t in seq(day.start, day.end, by = 15)) {
          curr.start <- t
          curr.end <- min((t + 14), day.end)
          if((curr.end - curr.start) > 9) {
               temp.result <- data.frame(PID = PID, Day = day, times.of.day = NA,
                                         a1.avg = NA, a1.std = NA, a1.cv = NA, a1.energy = NA, steps = NA)
               time.of.day <- private.whichTimeOfDay(accelerometer.data$TimeStamp[curr.start])
               temp.result$times.of.day = time.of.day
               temp.result$a1.avg <- private.meanAC(accelerometer.data$axis1[curr.start:curr.end])
               temp.result$a1.std <- private.stdAC(accelerometer.data$axis1[curr.start:curr.end])
               temp.result$a1.cv <- private.cvAC(temp.result$a1.avg, temp.result$a1.std)
               temp.result$a1.energy <- private.energyAC(accelerometer.data$axis1[curr.start:curr.end])
               temp.result$steps <- private.steps(accelerometer.data$steps[curr.start:curr.end])
               result <- data.frame(rbind(result, temp.result))
          }
          
     }
     result
}

private.meanAC <- function(axis1) {
     mean(axis1, na.rm = T)
}

private.stdAC <- function(axis1) {
     sd(axis1, na.rm = T)
}

private.cvAC <- function(avg, std) {
     result <- 0
     if(avg > 0) {
          result <- (std/avg) * 100
     }
     result
}

private.energyAC <- function(axis1) {
     result <- sum(axis1^2)
     result
}

private.steps <- function(step) {
     result <- sum(step, na.rm = T)
     result
}

private.smoothAxis <- function(axis1, threshold = 0.95) {
     t <- quantile(axis1, threshold, na.rm = T)[[1]]
     result <- axis1
     if(t > 0) {
          reject.idx <- which(axis1 > t)
          if(length(reject.idx) > 0) {
               result[reject.idx] <- 0
          }
     }
     result
}


private.whichTimeOfDay <- function(timeStamp) {
     times.of.day <- data.frame(title = factor(c("morning", "noon", "afternoon", "evening"), levels = c("morning", "noon", "afternoon", "evening")),
                                start.time = factor(c("08:00:00", "11:00:00", "14:00:00", "17:00:00"), levels = c("08:00:00", "11:00:00", "14:00:00", "17:00:00")),
                                end.time = factor(c("10:59:59", "13:59:59", "16:59:59", "19:59:59"), levels = c("10:59:59", "13:59:59", "16:59:59", "19:59:59")))
     curr.time <- unlist(strsplit(as.character(timeStamp), split = " "))[2]
     if(strptime(curr.time, "%T") < strptime(as.character(times.of.day$end.time[1]), "%T")) {
          return(as.character(times.of.day$title[1]))
     } else if(strptime(curr.time, "%T") < strptime(as.character(times.of.day$end.time[2]), "%T")) {
          return(as.character(times.of.day$title[2]))
     } else if(strptime(curr.time, "%T") < strptime(as.character(times.of.day$end.time[3]), "%T")) {
          return(as.character(times.of.day$title[3]))
     } else {
          return(as.character(times.of.day$title[4]))
     }
}

