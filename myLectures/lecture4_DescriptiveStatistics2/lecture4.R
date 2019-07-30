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

# Means vs medians:
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

# Measures of variability:
df1 <- data.frame(x = seq(-10, 10, .01), y = dnorm(seq(-10, 10, .01), 0, 1), Variability = "Small")
df2 <- data.frame(x = seq(-10, 10, .01), y = dnorm(seq(-10, 10, .01), 0, 2), Variability = "Medium")
df3 <- data.frame(x = seq(-10, 10, .01), y = dnorm(seq(-10, 10, .01), 0, 4), Variability = "Large")
df0 <- rbind(df1, df2, df3)
png(file = "VarDens.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df0, aes(x = x, y = y, group = Variability, color = Variability)) + geom_line(lwd = 1.25) + theme_bw() +
  labs(y = "Relative Frequency") 
dev.off()

# Comparing heights:
df1 <- data.frame(x2 = c(rnorm(50000, 60, 2), rnorm(50000, 70, 2)), 
                  Sex = factor(c(rep("Female", 50000), rep("Male", 50000))), Variability = "Small Variability")
df2 <- data.frame(x2 = c(rnorm(50000, 60, 15), rnorm(50000, 70, 15)), 
                  Sex = factor(c(rep("Female", 50000), rep("Male", 50000))), Variability = "Large Variability")
df0 <- rbind(df1, df2)
df0$Variability <- factor(df0$Variability, levels = c("Small Variability", "Large Variability"))

# Bimodal showing group
png(filename = "histNormal2b.png", height = 4, width = 7, units = "in", res = 600)
ggplot(df0, aes(x = x2, y = ..density.., fill = Sex)) + 
  geom_histogram(bins = 60, color = "black", alpha = .5, position = "identity") + 
  geom_vline(xintercept = 60, color = "darkred") + geom_vline(xintercept = 70, color = "darkblue") +
  facet_wrap(~Variability) + theme_bw() + labs(x = "Height (Inches)", y = "Relative Frequency") +
  ggtitle("Data distribution of the height of some people") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

########### Measures of spread ###########
pTile <- function(i, n) 100*(i - 0.5)/n
sapply(1:8, function(x) pTile(x, 8))
