############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)
library(ggrepel)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture6_RandomVariablesProbDist"))

############ Roll of two die ############
df1 <- expand.grid(a = 1:7, b = 1:7)
df1$Sum <- df1$a + df1$b
df2 <- df1 %>% group_by(Sum) %>% summarize(ways = n())
df2$relFreq <- df2$ways / sum(df2$ways)

png(file = "diceRollPDF.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df2, aes(x = Sum, ymin = 0, ymax = relFreq, y = relFreq)) + geom_pointrange() + theme_bw() + 
  labs(x = "y (Sum of two dice)", y = "P(y)") + ggtitle("Probability Distribution Function for Dice Roll Sum")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

set.seed(3)
a <- sample(1:7, 10000, replace = TRUE)
set.seed(33)
b <- sample(1:7, 10000, replace = TRUE)
df3 <- data.frame(Sum = a + b)
df3$a <- "a"

png(file = "diceRollHist1.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df3[1:100,], aes(x = Sum, y = (..count..)/sum(..count..))) + 
  geom_histogram(binwidth = 1, color = "grey20", fill = "grey80") + 
  geom_pointrange(data = df2, mapping = aes(x = Sum, ymin = 0, ymax = relFreq, y = relFreq), color = "darkblue") + 
  theme_bw() + ylab("Relative Frequency") + ggtitle("100 simulated dice rolls")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(file = "diceRollHist2.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df3[1:200,], aes(x = Sum, y = (..count..)/sum(..count..))) + 
  geom_histogram(binwidth = 1, color = "grey20", fill = "grey80") + 
  geom_pointrange(data = df2, mapping = aes(x = Sum, ymin = 0, ymax = relFreq, y = relFreq), color = "darkblue") + theme_bw() +
  ylab("Relative Frequency") + ggtitle("200 simulated dice rolls")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(file = "diceRollHist3.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df3[1:1000,], aes(x = Sum, y = (..count..)/sum(..count..))) + 
  geom_histogram(binwidth = 1, color = "grey20", fill = "grey80") + 
  geom_pointrange(data = df2, mapping = aes(x = Sum, ymin = 0, ymax = relFreq, y = relFreq), color = "darkblue") + theme_bw() +
  ylab("Relative Frequency") + ggtitle("1000 simulated dice rolls")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(file = "diceRollHist4.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df3, aes(x = Sum, y = (..count..)/sum(..count..))) + 
  geom_histogram(binwidth = 1, color = "grey20", fill = "grey80") + 
  geom_pointrange(data = df2, mapping = aes(x = Sum, ymin = 0, ymax = relFreq, y = relFreq), color = "darkblue") + theme_bw() +
  ylab("Relative Frequency") + ggtitle("10000 simulated dice rolls")+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
