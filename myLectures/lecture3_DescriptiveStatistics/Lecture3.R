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
