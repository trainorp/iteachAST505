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

# Empirical rule:
sum(as.numeric(fishweightsDF$Weight < (mean(fishweightsDF$Weight) - sd(fishweightsDF$Weight))))
sum(as.numeric(fishweightsDF$Weight > (mean(fishweightsDF$Weight) + sd(fishweightsDF$Weight))))

png(file = "fishEmp00.png", height = 4, width = 6, units = "in", res = 600)
ggplot(fishweightsDF, aes(x = Weight)) + geom_histogram(breaks = seq(470, 535, 5), color = "black", fill = "grey65") +
  ggtitle("Weight of fish in NMSU pond") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ylab("Frequency")
dev.off()
png(file = "fishEmp0.png", height = 4, width = 6, units = "in", res = 600)
ggplot(fishweightsDF, aes(x = Weight)) + geom_histogram(breaks = seq(470, 535, 5), color = "black", fill = "grey65") +
  geom_vline(xintercept = mean(fishweightsDF$Weight), lwd = 1.5, color = "darkred") + 
  geom_vline(xintercept = mean(fishweightsDF$Weight) - sd(fishweightsDF$Weight), lwd = 1.5, color = "darkblue") + 
  geom_vline(xintercept = mean(fishweightsDF$Weight) + sd(fishweightsDF$Weight), lwd = 1.5, color = "darkblue") + 
  ggtitle("Weight of fish in NMSU pond") + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + ylab("Frequency")
dev.off()
png(file = "fishEmp.png", height = 4, width = 6, units = "in", res = 600)
ggplot(fishweightsDF, aes(x = Weight)) + geom_histogram(breaks = seq(470, 535, 5), color = "black", fill = "grey65") +
  geom_vline(xintercept = mean(fishweightsDF$Weight), lwd = 1.5, color = "darkred") + 
  geom_vline(xintercept = mean(fishweightsDF$Weight) - sd(fishweightsDF$Weight), lwd = 1.5, color = "darkblue") + 
  geom_vline(xintercept = mean(fishweightsDF$Weight) + sd(fishweightsDF$Weight), lwd = 1.5, color = "darkblue") + 
  annotate("label", x = 480, y = 17, label = "14", size = 12) +
  annotate("label", x = 520, y = 17, label = "15", size = 12) +
  annotate("label", x = 500, y = 17, label = "71", size = 12) +
  ggtitle("Weight of fish in NMSU pond") + 
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

states$pay[order(states$pay)]
quantile(states$pay, probs = c(.25, .75))
quantile(states$pay, probs = c(.25, .75), type = 2)
IQR(states$pay, type = 2)

# Variance:
sum((states$pay - mean(states$pay))**2)/7

# CV:
sd(c(10.03, 9.98, 10.01, 9.97)) / mean(c(10.03, 9.98, 10.01, 9.97))

# MAD:
mad(states$pay)
median(abs(states$pay - median(states$pay)))/.6745

boxplot(states$pay)

########### Boxplots ###########
set.seed(333)
x <- rexp(10)
x[3] <- 3.8533753
x[7] <- 2.7955990
x <- x[order(x)]
x <- round(x, 2)
quants <- quantile(x, probs = c(.25, .5, .75), type = 2)
iqr <- quants[3] - quants[1]
lif <- quants[1] - 1.5 * iqr
lof <- quants[1] - 3 * iqr
uif <- quants[3] + 1.5 * iqr
uof <- quants[3] + 3 * iqr
boxplot(x)
xVals <-x

dfBox <- data.frame(xmin = -.5, xmax = .5, ymin = quants[1], ymax = quants[3])

png(filename = "saltRiver1.png", width = 6, height = 4, units = "in", res = 600)
ggplot(dfBox, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + 
  geom_rect(fill = "grey90", color = "black", lwd = 1) + annotate("label", x = .6, y = quants[1], label = "Q1") +
  annotate("label", x = .6, y = quants[3], label = "Q3") + xlim(-1, 1) + ylim(0, 5) + 
  theme_bw() + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("Cubic meters / second") + ggtitle("Flow Rate of the Salt River, Kentucky") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "saltRiver2.png", width = 6, height = 4, units = "in", res = 600)
ggplot(dfBox, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + 
  geom_rect(fill = "grey90", color = "black", lwd = 1) + 
  geom_segment(aes(x = xmin, xend = xmax, y = 1, yend = 1), lwd = 1) +
  annotate("label", x = .6, y = quants[1], label = "Q1") + annotate("label", x = .6, y = quants[3], label = "Q3") + 
  xlim(-1, 1) + ylim(0, 5) + theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("Cubic meters / second") + ggtitle("Flow Rate of the Salt River, Kentucky") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "saltRiver3.png", width = 6, height = 4, units = "in", res = 600)
ggplot(dfBox, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + 
  geom_rect(fill = "grey90", color = "black", lwd = 1) + 
  geom_segment(aes(x = xmin, xend = xmax, y = 1, yend = 1), lwd = 1) +
  geom_segment(aes(x = 0, xend = 0, y = quants[1], yend = xVals[1]), lwd = 1) +
  annotate("label", x = .6, y = quants[1], label = "Q1") + annotate("label", x = .6, y = quants[3], label = "Q3") + 
  xlim(-1, 1) + ylim(0, 5) + theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("Cubic meters / second") + ggtitle("Flow Rate of the Salt River, Kentucky") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "saltRiver4.png", width = 6, height = 4, units = "in", res = 600)
ggplot(dfBox, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + 
  geom_rect(fill = "grey90", color = "black", lwd = 1) + 
  geom_segment(aes(x = xmin, xend = xmax, y = 1, yend = 1), lwd = 1) +
  geom_segment(aes(x = 0, xend = 0, y = quants[1], yend = xVals[1]), lwd = 1) +
  annotate("label", x = .6, y = quants[1], label = "Q1") + annotate("label", x = .6, y = quants[3], label = "Q3") + 
  annotate("point", x = 0, y = xVals[9], size = 2, shape = 1, color = "black") +
  annotate("point", x = 0, y = xVals[10], size = 2, shape = 16, color = "black") +
  xlim(-1, 1) + ylim(0, 5) + theme_bw() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab("Cubic meters / second") + ggtitle("Flow Rate of the Salt River, Kentucky") + theme(plot.title = element_text(hjust = 0.5))
dev.off()
