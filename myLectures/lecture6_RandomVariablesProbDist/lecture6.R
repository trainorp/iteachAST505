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
paste0("(", df1$a, ",", df1$b, ")")
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

########### Tick example ###########
dbinom(0, 10, .18) + dbinom(1, 10, .18) + dbinom(2, 10, .18)
1 - pbinom(2, 10, .18)

set.seed(333)
df4 <- data.frame(x = rbinom(100000, 100, .18))

png(file = "TickBinom.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df4, aes(x = x, y = (..count..)/sum(..count..))) + 
  geom_histogram(breaks = 0:100, color = "black", fill = "grey70") + 
  xlim(0, 40) + labs(x = "y (# of Ticks with Bacteria)", y = "Relative Frequency") + theme_bw() +
  ggtitle(expression(paste('Tick binomial experiment: ', n == 100, ", ", pi == 0.18)))+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

sqrt(100*.18*(1-.18))

png(file = "TickBinom2.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df4, aes(x = x, y = (..count..)/sum(..count..))) + 
  geom_histogram(breaks = 0:100, color = "black", fill = "grey70") + 
  geom_segment(aes(x = 18, xend = 18 + sqrt(100*.18*(1-.18)), y = .005, yend = .005), color = "darkred", lwd = 1.5) +
  geom_segment(aes(x = 18, xend = 18 - sqrt(100*.18*(1-.18)), y = .005, yend = .005), color = "darkred", lwd = 1.5) +
  geom_vline(xintercept = 18, color = "darkblue", lwd = 1.5) +
  xlim(0, 40) + labs(x = "y (# of Ticks with Bacteria)", y = "Relative Frequency") + theme_bw() +
  ggtitle(expression(paste('Tick binomial experiment: ', n == 100, ", ", pi == 0.18)))+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


18 + 2 * sqrt(100*.18*(1-.18))


########### Continuous RV's ###########
png(file = "NormalHeights.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(24, 104)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x, 64, 8), xlim = c(55, 60), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x, 64, 8), lwd = 1, n = 50000) + 
  theme_bw() + labs(x = "y (Height in inches)", y = expression(f(y))) +
  ggtitle("Probability distribution function for heights") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(file = "StandardNorm1.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-5, 5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(-1, 1), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000) + 
  scale_x_continuous(breaks = c(-1, 0, 1), labels = c(expression(-sigma==-1), expression(mu==0), expression(sigma==1))) +
  theme_bw() + labs(x = "y", y = expression(f(y))) +
  annotate("label", 0, 0.1, label = "68.27%\n or 0.6827") +
  ggtitle("The Standard Normal Distribution") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(file = "StandardNorm2.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-5, 5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(-2, 2), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000) + 
  scale_x_continuous(breaks = c(-2, 0, 2), labels = c(expression(-2*sigma==-2), expression(mu==0), expression(2*sigma==2))) +
  theme_bw() + labs(x = "y", y = expression(f(y))) +
  annotate("label", 0, 0.1, label = "95.45%\n or 0.9545") +
  ggtitle("The Standard Normal Distribution") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(file = "StandardNorm3.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-5, 5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(-3, 3), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000) + 
  scale_x_continuous(breaks = c(-3, 0, 3), labels = c(expression(-3*sigma==-3), expression(mu==0), expression(3*sigma==3))) +
  theme_bw() + labs(x = "y", y = expression(f(y))) +
  annotate("label", 0, 0.1, label = "99.73%\n or 0.9973") +
  ggtitle("The Standard Normal Distribution") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(file = "StandardNorm4.png", height = 4, width = 6, units = "in", res = 600)
p1 <- ggplot(data.frame(x = c(-5, 5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(-5, 1), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000) + xlim(-3, 3) +
  scale_x_continuous(breaks = c(1), labels = c(expression(z==1))) +
  theme_bw() + labs(x = "z", y = "Normal density") +
  annotate("label", 0, 0.15, label = expression(P(Z <= z)==0.8413)) +
  ggtitle("The Standard Normal Distribution") +
  theme(plot.title = element_text(hjust = 0.5))
show(p1)
dev.off()

p2 <- ggplot(data.frame(x = c(-5, 5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(1, 5), geom = "area", fill = "hotpink", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000) + xlim(-3, 3) +
  scale_x_continuous(breaks = c(1), labels = c(expression(z==1))) +
  theme_bw() + labs(x = "z", y = "Normal density") +
  annotate("label", 3.7, 0.175, label = expression(P(Z > z)==1-P(Z<=z))) +
  annotate("label", 3.7, 0.125, label = expression(1-P(Z<=z)==1-0.8413)) +
  annotate("label", 3.7, 0.075, label = expression(1-P(Z<=z)==0.1587)) +
  ggtitle("The Standard Normal Distribution") +
  theme(plot.title = element_text(hjust = 0.5))

png(file = "StandardNorm5.png", height = 7, width = 6, units = "in", res = 600)
print(gridExtra::grid.arrange(p1, p2))
dev.off()

png(file = "StandardNorm6.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-5, 5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(0, 1), geom = "area", fill = "limegreen", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000) + xlim(-3, 3) +
  scale_x_continuous(breaks = c(0, 1), labels = c(expression(z[1]==0), expression(z[2]==1))) +
  theme_bw() + labs(x = "z", y = "Normal density") +
  annotate("label", 3.7, 0.175, label = expression(P(z[2] <= Z)-P(z[1] <= Z))) +
  annotate("label", 3.7, 0.125, label = expression(P(z[2] <= 1)-P(z[1] <= 0))) +
  annotate("label", 3.7, 0.075, label = expression(0.841-0.500 == 0.341)) +
  ggtitle("The Standard Normal Distribution") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

########### Trout ###########
(5.00-4.03)/(1.65)
pnorm((5.00-4.03)/(1.65))

