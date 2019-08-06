############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)
library(ggrepel)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture5_EventsProbabilityLaws"))

############ Cards ############
set.seed(3)
df1 <- data.frame(Suite = factor(sample(c("Clubs", "Diamonds", "Hearts", "Spades"), 1000000, replace = TRUE)))
df1$a <- "a"
prop.table(table(x))

png(filename = "bar1.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df1[1:1,], aes(x = Suite, fill = Suite)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  geom_hline(yintercept = .25, lwd = 1.1) + ylim(0, 1) + 
  scale_fill_discrete(drop = FALSE) + scale_x_discrete(drop = FALSE) + theme_bw() + ylab("Relative frequency") +
  ggtitle("One draw") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "bar2.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df1[1:2,], aes(x = Suite, fill = Suite)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  geom_hline(yintercept = .25, lwd = 1.1) + ylim(0, 1) + 
  scale_fill_discrete(drop = FALSE) + scale_x_discrete(drop = FALSE) + theme_bw() + ylab("Relative frequency") +
  ggtitle("Two draws") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "bar3.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df1[1:4,], aes(x = Suite, fill = Suite)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  geom_hline(yintercept = .25, lwd = 1.1) + ylim(0, 1) + 
  scale_fill_discrete(drop = FALSE) + scale_x_discrete(drop = FALSE) + theme_bw() + ylab("Relative frequency") +
  ggtitle("Four draws") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "bar4.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df1[1:10,], aes(x = Suite, fill = Suite)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  geom_hline(yintercept = .25, lwd = 1.1) + ylim(0, 1) + 
  scale_fill_discrete(drop = FALSE) + scale_x_discrete(drop = FALSE) + theme_bw() + ylab("Relative frequency") +
  ggtitle("One draw") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "bar5.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df1[1:100,], aes(x = Suite, fill = Suite)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  geom_hline(yintercept = .25, lwd = 1.1) + ylim(0, 1) + 
  scale_fill_discrete(drop = FALSE) + scale_x_discrete(drop = FALSE) + theme_bw() + ylab("Relative frequency") +
  ggtitle("100 draws") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "bar6.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df1[1:1000,], aes(x = Suite, fill = Suite)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  geom_hline(yintercept = .25, lwd = 1.1) + ylim(0, 1) + 
  scale_fill_discrete(drop = FALSE) + scale_x_discrete(drop = FALSE) + theme_bw() + ylab("Relative frequency") +
  ggtitle("1,000 draws") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "bar7.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df1[1:10000,], aes(x = Suite, fill = Suite)) + geom_bar(aes(y = (..count..)/sum(..count..))) + 
  geom_hline(yintercept = .25, lwd = 1.1) + ylim(0, 1) + 
  scale_fill_discrete(drop = FALSE) + scale_x_discrete(drop = FALSE) + theme_bw() + ylab("Relative frequency") +
  ggtitle("10,000 draws") + theme(plot.title = element_text(hjust = 0.5))
dev.off()

############ Union and intersection data ############
data("Titanic")
titanicDF <- as.data.frame(Titanic)
titanicDF$Class2 <- ifelse(titanicDF$Class == "1st", "1st", "Not 1st")
titanicDF %>% group_by(Class2, Survived) %>% summarize(sum(Freq))
titanicDF %>% group_by(Class2) %>% summarize(sum(Freq))
titanicDF %>% group_by(Survived) %>% summarize(sum(Freq))
