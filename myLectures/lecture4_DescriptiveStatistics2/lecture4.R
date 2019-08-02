############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)
library(ggrepel)

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

# Two group boxplot example:
data("plasma", package = "HSAUR")
png(file = "fibrinogen.png", width = 6, height = 4, units = "in", res = 600)
ggplot(plasma, aes(y = fibrinogen, x = ESR, fill = ESR)) + geom_boxplot(lwd = .9) + theme_bw() +
  labs(x = "Erythrocyte sedimentation rate", y = "Fibrinogen concentration") + 
  scale_fill_manual(values = c("indianred", "dodgerblue")) +
  ggtitle("Fibrinogen in plasma by ESR") + theme(plot.title = element_text(hjust = 0.5))
dev.off()


########### Scatterplots & Correlation ###########
data("cd4", package = "boot")

png(file = "cd4.png", width = 5, height = 4, units = "in", res = 600)
ggplot(cd4, aes(x = baseline, y = oneyear)) + geom_point() + theme_bw() + 
  labs(x = "Baseline count", y = "Post-treatment count") + 
  ggtitle("CD4+ cell count") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

########### Correlation examples ###########
gmc <- function(corr){
  return(matrix(c(1, corr, corr, 1), nrow = 2, ncol = 2))
}

corList <- c(-.99, -.9, -.75, -.5, 0, .5, .75, .9, .99)
matList <- list()
for(i in 1:length(corList)){
  set.seed(333)
  matList[[i]] <- MASS::mvrnorm(n = 300, mu = c(0, 0), Sigma = gmc(corList[i]))
}
mats <- do.call("rbind", matList)
mats <- as.data.frame(mats)
mats$Correlation <- rep(corList, each = 300)

png(file = "scatter.png", width = 7, height = 6, units = "in", res = 600)
ggplot(mats, aes(x = V1, y = V2)) + geom_point(size = 1) + facet_wrap(~Correlation, nrow = 3, ncol = 3) +
  theme_bw() + labs(x = "x", y = "y") + ggtitle("Scatterplots for showing different correlations") + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

sx <- sd(states$pay)
xbar <- mean(states$pay)
sy <- sd(states$SATM)
ybar <- mean(states$SATM)
states$pay - xbar
states$SATM - ybar

(states$pay - xbar) * (states$SATM - ybar)
sum((states$pay - xbar) * (states$SATM - ybar))
sum((states$pay - xbar) * (states$SATM - ybar)) / (sx * sy * 7)
cor(states$pay, states$SATM)

states$State <- rownames(states)
png(file = "teacherPay.png", height = 4, width = 6, units = "in", res = 600)
set.seed(33)
ggplot(states, aes(x = pay, y = SATM, label = State)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text_repel() + theme_bw() +
  labs(x = "Teacher Pay (1,000's of $)", y = "SAT Math") + 
  annotate("label", x = 27, y = 500, label ="italic(r)==-0.657", parse = TRUE) +
  ggtitle("Teacher pay and SAT math score") + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

states2 <- carData::States
ggplot(states2, aes(x = log10(pop), y = SATM)) + geom_point() + geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

states2$State <- rownames(states2)
cor(states2$percent[states2$region == "MTN"], states2$SATM[states2$region == "MTN"])
cor(states2$percent[states2$region == "MTN"], states2$pay[states2$region == "MTN"])

png(file = "teacherPay2.png", height = 4, width = 8, units = "in", res = 600)
set.seed(33)
p1 <- ggplot(states2 %>% filter(region == "MTN"), aes(x = percent, y = SATM, label = State)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + geom_text_repel() + theme_bw() + 
  labs(x = "Percent of high schoolers who will take the SAT (%)", y = "SAT Math") +
  annotate("label", x = 10, y = 500, label ="italic(r)==-0.741", parse = TRUE) +
  ggtitle("% test takers & SAT math score") + theme(plot.title = element_text(hjust = 0.5))

set.seed(33)
p2 <- ggplot(states2 %>% filter(region == "MTN"), aes(x = pay, y = percent, label = State)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + geom_text_repel() + theme_bw() + 
  labs(y = "Percent of high schoolers who will take the SAT (%)", x = "Teacher Pay") +
  annotate("label", x = 31, y = 10, label ="italic(r)==0.737", parse = TRUE) +
  ggtitle("Teacher pay & % test takers") + theme(plot.title = element_text(hjust = 0.5))

gridExtra::grid.arrange(p1, p2, nrow = 1)
dev.off()

png(file = "teacherPay3.png", height = 4, width = 10, units = "in", res = 600)
set.seed(33)
p1 <- ggplot(states2 %>% filter(region == "MTN"), aes(x = percent, y = SATM, label = State)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + geom_text_repel() + theme_bw() + 
  labs(x = "Percent test takers (%)", y = "SAT Math") +
  annotate("label", x = 10, y = 500, label ="italic(r)==-0.741", parse = TRUE) 
set.seed(33)
p2 <- ggplot(states2 %>% filter(region == "MTN"), aes(x = pay, y = percent, label = State)) + geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + geom_text_repel() + theme_bw() + 
  labs(y = "Percent test takers (%)", x = "Teacher Pay") +
  annotate("label", x = 31, y = 10, label ="italic(r)==0.737", parse = TRUE)
set.seed(33)
p3 <- ggplot(states, aes(x = pay, y = SATM, label = State)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) + geom_text_repel() + theme_bw() +
  labs(x = "Teacher Pay (1,000's of $)", y = "SAT Math") + 
  annotate("label", x = 27, y = 500, label ="italic(r)==-0.657", parse = TRUE)
gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
dev.off()
