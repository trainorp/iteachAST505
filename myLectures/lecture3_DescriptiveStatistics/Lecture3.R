library(tidyverse)

setwd("~/gdrive/iTeach/AST_505/myLectures/lecture3_DescriptiveStatistics")
data("Titanic")

Titanic2 <- as.data.frame(Titanic)
Titanic3 <- epitools::expand.table(Titanic)

TitanicCSTab <- xtabs(~Class + Sex, data = Titanic3)
TitanicCSDF <- as.data.frame(TitanicCSTab)
TitanicCSDF2 <- epitools::expand.table(TitanicCSTab)
TitanicCSDF$relFreq <- TitanicCSDF$Freq / sum(TitanicCSDF$Freq)
TitanicCSDF2$classSex <- paste0(TitanicCSDF2$Class, ", ", TitanicCSDF2$Sex)
TitanicCSDF$classSex <- paste0(TitanicCSDF$Class, ", ", TitanicCSDF$Sex)

png(filename = "TitanicClassSex.png", height = 4, width = 6.5, units = "in", res = 200)
ggplot(TitanicCSDF2, aes(x = classSex)) + geom_bar() + theme_bw() + labs(x = "Class, Sex", y = "Frequency") +
  ggtitle("Ticket and Sex of persons on the Titanic") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "TitanicPieChart.png", height = 5, width = 5, units = "in", res = 200)
ggplot(TitanicCSDF, aes(x = "", fill = classSex, y = relFreq)) + geom_bar(width = 1, stat = "identity", color = "white") + 
  coord_polar("y", start = 0) + theme_void() + labs(fill = "Class, Sex", y = "Frequency") + 
  ggtitle("Ticket and Sex of persons on the Titanic") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "TitanicClassSex2.png", height = 4, width = 6.5, units = "in", res = 200)
ggplot(TitanicCSDF, aes(x = "", fill = classSex, y = relFreq)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 1.1)) + theme_light() + coord_flip() + 
  labs(fill = "Class, Sex", x = "Class, Sex", y = "Relative Frequency / Proportion") +
  ggtitle("Ticket and Sex of persons on the Titanic") + theme(plot.title = element_text(hjust = 0.5))
dev.off()
