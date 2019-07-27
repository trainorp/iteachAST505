############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture3_DescriptiveStatistics"))

########### Titanic dataset ############
data("Titanic")
Titanic2 <- as.data.frame(Titanic)
Titanic3 <- epitools::expand.table(Titanic)

TitanicCSTab <- xtabs(~Class + Sex, data = Titanic3)
TitanicCSDF <- as.data.frame(TitanicCSTab)
TitanicCSDF2 <- epitools::expand.table(TitanicCSTab)
TitanicCSDF$relFreq <- TitanicCSDF$Freq / sum(TitanicCSDF$Freq)
TitanicCSDF2$classSex <- paste0(TitanicCSDF2$Class, ", ", TitanicCSDF2$Sex)
TitanicCSDF$classSex <- paste0(TitanicCSDF$Class, ", ", TitanicCSDF$Sex)

# Bar plot
png(filename = "TitanicClassSex.png", height = 4, width = 6.5, units = "in", res = 600)
ggplot(TitanicCSDF2, aes(x = classSex)) + geom_bar() + theme_bw() + labs(x = "Class, Sex", y = "Frequency") +
  ggtitle("Ticket and Sex of persons on the Titanic") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

# Stupid pie chart
png(filename = "TitanicPieChart.png", height = 5, width = 5, units = "in", res = 600)
ggplot(TitanicCSDF, aes(x = "", fill = classSex, y = relFreq)) + geom_bar(width = 1, stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + theme_void() + labs(fill = "Class, Sex", y = "Frequency") + 
  ggtitle("Ticket and Sex of persons on the Titanic") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

# Relative frequency bar chart
png(filename = "TitanicClassSex3.png", height = 4, width = 6.5, units = "in", res = 600)
ggplot(TitanicCSDF, aes(x = classSex, y = relFreq)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 1.1)) + theme_light() + 
  labs(fill = "Class, Sex", x = "Class, Sex", y = "Relative Frequency / Proportion") +
  ggtitle("Ticket and Sex of persons on the Titanic") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

# Same as a percent
png(filename = "TitanicClassSex3b.png", height = 4, width = 6.5, units = "in", res = 600)
ggplot(TitanicCSDF, aes(x = classSex, y = relFreq*100)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 1.1)) + theme_light() + 
  labs(fill = "Class, Sex", x = "Class, Sex", y = "Percent of Total (%)") +
  ggtitle("Ticket and Sex of persons on the Titanic") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

# Beautified
png(filename = "TitanicClassSex2.png", height = 4, width = 6.5, units = "in", res = 600)
ggplot(TitanicCSDF, aes(x = "", fill = classSex, y = relFreq)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 1.1)) + theme_light() + coord_flip() + 
  labs(fill = "Class, Sex", x = "Class, Sex", y = "Relative Frequency / Proportion") +
  ggtitle("Ticket and Sex of persons on the Titanic") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

########### Prostate cancer dataset ############
stagec <- rpart::stagec
stagec <- stagec[!is.na(stagec$g2),]
stagec <- stagec %>% mutate(classInterval = cut(g2, seq(2, 56, 6)))

stagecSum <- stagec %>% group_by(classInterval) %>% summarize(freq = n())
stagecSum$relFreq <- stagecSum$freq / sum(stagecSum$freq)
stagecSum <- as.data.frame(stagecSum)
round(stagecSum$relFreq, 3)

png(filename = "g2Hist.png", height = 4, width = 6, units = "in", res = 600)
ggplot(stagec, aes(x = g2)) + geom_histogram(breaks = seq(2, 56, 6), col = "black", fill = "grey65") + 
  scale_x_continuous("Percentage of cells in G2 phase", seq(2, 56, 6)) + theme_bw() + labs(y = "Frequency")
dev.off()

png(filename = "g2Hist2.png", height = 4, width = 6, units = "in", res = 600)
ggplot(stagec, aes(x = g2, y = 6*..density..)) + 
  geom_histogram(breaks = seq(2, 56, 6), col = "black", fill = "grey65") + 
  scale_x_continuous("Percentage of cells in G2 phase", seq(2, 56, 6)) + theme_bw() + labs(y = "Relative Frequency")
dev.off()

########### Prostate cancer dataset ############
df1 <- data.frame(x1 = rnorm(100000, 67, 5), x2 = c(rnorm(50000, 60, 2.5),rnorm(50000, 70, 2.5)), 
                  Sex = factor(c(rep("Female", 50000), rep("Male", 50000))))

# Unimodal:
png(filename = "histNormal.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df1, aes(x = x1)) + geom_histogram(bins = 30, color = "black", fill = "grey65") + 
  theme_bw() + labs(x = "Height (Inches)", y = "Frequency") +
  ggtitle("Data distribution of the height of some people") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

# Bimodal:
png(filename = "histNormal2.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df1, aes(x = x2)) + geom_histogram(bins = 30, color = "black", fill = "grey65") + 
  theme_bw() + labs(x = "Height (Inches)", y = "Frequency") +
  ggtitle("Data distribution of the height of some people") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

# Bimodal showing group
png(filename = "histNormal2b.png", height = 4, width = 6.5, units = "in", res = 600)
ggplot(df1, aes(x = x2, fill = Sex)) + geom_histogram(bins = 30, color = "black", alpha = .5, position = "identity") + 
  theme_bw() + labs(x = "Height (Inches)", y = "Frequency") +
  ggtitle("Data distribution of the height of some people") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

# Symmetric:
png(filename = "histNormalSym.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df1, aes(x = x1)) + geom_histogram(bins = 30, color = "black", fill = "grey65") + 
  geom_vline(xintercept = 67, color = "darkblue", lwd = 2) +
  theme_bw() + labs(x = "Height (Inches)", y = "Frequency") +
  ggtitle("Data distribution of the height of some people") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

# Generate some beta RV's for showing right- and left-tailed distributions:
betaMode <- function(alpha, beta) (alpha - 1)/(alpha + beta - 2)
df2 <- data.frame(x1 = rbeta(100000, 3, 11))
df2$x2 <- 1 - df2$x1

png(file = "histRightSkew.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df2, aes(x = x1)) + geom_histogram(bins = 30, color = "black", fill = "grey65") + 
  geom_vline(xintercept = betaMode(3, 12), color = "darkblue", lwd = 2) + theme_bw() + 
  labs(x = "Weight (kg)", y = "Frequency") + ggtitle("Weight of some fish") + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(file = "histLeftSkew.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df2, aes(x = x2)) + geom_histogram(bins = 30, color = "black", fill = "grey65") + 
  geom_vline(xintercept = 1 - betaMode(3, 11), color = "darkblue", lwd = 2) + theme_bw() +
  labs(x = "Weight (kg)", y = "Frequency") + ggtitle("Weight of some other fish") + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
