############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture4_DescriptiveStatistics2"))


states <- carData::States
states <- states[states$region == "MTN", c("SATM", "dollars", "pay")]
table(states$pay)

median(states$pay)

# Relationship between the population mean and sample mean
set.seed(33)
fishWeights <- rnorm(100, 500, 10)
fishweights <- round(fishWeights, 2)
mean(fishweights)
set.seed(55)
fishweightsSamp <- sample(fishweights, 10)
mean(fishweightsSamp)

# Trimmed means:
fishweightsDF <- data.frame(Weight = fishweights)
fishweightsDF2 <- fishweightsDF
fishweightsDF2$Weight[95:100] <- 610

png(file = "fishweight.png", height = 4, width = 6, units = "in", res = 600)
ggplot(fishweightsDF, aes(x = Weight)) + geom_histogram(breaks = seq(450, 615, 5), color = "black", fill = "grey65") + 
  geom_vline(xintercept = mean(fishweights), color = "darkred", lwd = 1.5) +
  theme_bw() + ylab("Frequency") + ggtitle("Weight of fish in NMSU pond (mean in Red)") + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(file = "fishweightMeans.png", height = 4, width = 6, units = "in", res = 600)
ggplot(fishweightsDF2, aes(x = Weight)) + geom_histogram(breaks = seq(450, 615, 5), color = "black", fill = "grey65") + 
  geom_vline(xintercept = mean(fishweights), color = "darkred", lwd = 1.5) +
  geom_vline(xintercept = mean(fishweightsDF2$Weight), color = "darkblue", lwd = 1.5) +
  ggtitle("Weight of fish in NMSU pond (Old mean in Red; New in Blue)") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ylab("Frequency")
dev.off()

png(file = "fishweightMedians.png", height = 4, width = 6, units = "in", res = 600)
ggplot(fishweightsDF2, aes(x = Weight)) + geom_histogram(breaks = seq(450, 615, 5), color = "black", fill = "grey65") + 
  geom_vline(xintercept = median(fishweights), color = "darkred", lwd = 1.5) +
  geom_vline(xintercept = median(fishweightsDF2$Weight), color = "darkblue", lwd = 1.5) +
  ggtitle("Weight of fish in NMSU pond (Old median in Red; New in Blue)") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ylab("Frequency")
dev.off()