############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)
library(ggrepel)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture8_MeanInference"))

############ Coverage probabilities ############
set.seed(3)
df1 <- data.frame(x = rnorm(10000))

png(file = "coverage1.png", height = 4, width = 6.5, units = "in", res = 600)
set.seed(3333)
xbar <- replicate(100, mean(sample(df1$x, 25)))
lower <- xbar - qnorm(.975) * 1 / sqrt(25)
upper <- xbar + qnorm(.975) * 1 / sqrt(25)
df2 <- data.frame(xbar = xbar, lower = lower, upper = upper, x = 1:length(xbar))
df2$contains <- ifelse(df2$lower < 0 & df2$upper > 0, "Yes", "No")
ggplot(df2, aes()) + geom_segment(aes(x = x, xend = x, y = lower, yend = upper, color = contains)) +
  geom_point(aes(x = x, y = xbar, color = contains)) + geom_hline(yintercept = 0, color = "darkblue", lwd = 1.1) +
  scale_y_continuous(breaks = c(-1, 0, 1), labels = c(expression(mu - sigma), expression(mu), expression(mu + sigma)),
                     limits = c(-1.1, 1.1)) +
  theme_bw() + labs(x = "Random Sample #", y = "", color = expression(paste("Contains ", mu))) +
  annotate("label", x = 50, y = 1, label = "95 / 100 = 95%") +
  ggtitle(expression(paste("Confidence intervals from 100 random samples with ", 1-alpha == .95))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

png(file = "coverage2.png", height = 4, width = 6.5, units = "in", res = 600)
set.seed(333)
xbar <- replicate(100, mean(sample(df1$x, 25)))
lower <- xbar - qnorm(.975) * 1 / sqrt(25)
upper <- xbar + qnorm(.975) * 1 / sqrt(25)
df2 <- data.frame(xbar = xbar, lower = lower, upper = upper, x = 1:length(xbar))
df2$contains <- ifelse(df2$lower < 0 & df2$upper > 0, "Yes", "No")
ggplot(df2, aes()) + geom_segment(aes(x = x, xend = x, y = lower, yend = upper, color = contains)) +
  geom_point(aes(x = x, y = xbar, color = contains)) + geom_hline(yintercept = 0, color = "darkblue", lwd = 1.1) +
  scale_y_continuous(breaks = c(-1, 0, 1), labels = c(expression(mu - sigma), expression(mu), expression(mu + sigma)),
                     limits = c(-1.1, 1.1)) +
  theme_bw() + labs(x = "Random Sample #", y = "", color = expression(paste("Contains ", mu))) +
  annotate("label", x = 50, y = 1, label = "93 / 100 = 93%") +
  ggtitle(expression(paste("Confidence intervals from 100 random samples with ", 1-alpha == .95))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

png(file = "coverage3.png", height = 4, width = 6.5, units = "in", res = 600)
set.seed(33)
xbar <- replicate(100, mean(sample(df1$x, 25)))
lower <- xbar - qnorm(.975) * 1 / sqrt(25)
upper <- xbar + qnorm(.975) * 1 / sqrt(25)
df2 <- data.frame(xbar = xbar, lower = lower, upper = upper, x = 1:length(xbar))
df2$contains <- ifelse(df2$lower < 0 & df2$upper > 0, "Yes", "No")
ggplot(df2, aes()) + geom_segment(aes(x = x, xend = x, y = lower, yend = upper, color = contains)) +
  geom_point(aes(x = x, y = xbar, color = contains)) + geom_hline(yintercept = 0, color = "darkblue", lwd = 1.1) +
  scale_y_continuous(breaks = c(-1, 0, 1), labels = c(expression(mu - sigma), expression(mu), expression(mu + sigma)),
                     limits = c(-1.1, 1.1)) +
  theme_bw() + labs(x = "Random Sample #", y = "", color = expression(paste("Contains ", mu))) +
  annotate("label", x = 50, y = 1, label = "97 / 100 = 97%") +
  ggtitle(expression(paste("Confidence intervals from 100 random samples with ", 1-alpha == .95))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

########### Fish weights example ############
set.seed(33)
fishWeights <- rnorm(100, 500, 10)
fishweights <- round(fishWeights, 2)
mean(fishweights)

varp <- function(x) mean((x-mean(x))^2)
sqrt(varp(fishweights))

set.seed(33333)
samp <- sample(fishweights, 5)
mean(samp)

round(sd(samp),3)

########### 100 test statistics ############
set.seed(1111111)
df3 <- data.frame(x = replicate(200, mean(rnorm(10))), y = runif(200, 0, .1))
df3$Test <- "Null is true"

df4 <- data.frame(x = seq(-3, 2, .0001), y = dnorm(seq(-3, 2, .0001), mean = 0, sd = 1/sqrt(10)))

png(file = "nullTests.png", height = 3.5, width = 6, units = "in", res = 600)
ggplot(df4, aes(x = x, y = y)) + geom_line(lwd = 1) + geom_point(data = df3, aes(x = x, y = y, color = Test), size = .5) + 
  scale_x_continuous(breaks = c(0), labels = c(expression(mu[0]))) +
  labs(x = "", y = "Density", color = "Test Statistic") + theme_bw() +
  ggtitle("Test statistics from 100 samples given the null hypothesis is true") +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

png(file = "nullTests2.png", height = 3.5, width = 6, units = "in", res = 600)
ggplot(df4, aes(x = x, y = y)) + geom_line(lwd = 1) + geom_point(data = df3, aes(x = x, y = y, color = Test), size = .5) + 
  annotate("point", x = -2.4, y = 0.0175, color = "blue", size = 1.25) + 
  annotate("label", x = -2.4, y = 0.1, label = "A new sample") + 
  scale_x_continuous(breaks = c(0), labels = c(expression(mu[0]))) +
  ggtitle("100 TS under the null + 1 new one") +
  labs(x = "", y = "Density", color = "Test Statistic") + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

########### Rejection regions ############
png(file = "nullTests3.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-3.5, 3.5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(-3.4, -1.96), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000) + 
  geom_segment(aes(x = -1.96, xend = -1.96, y = 0, yend = dnorm(-1.96))) +
  scale_x_continuous(breaks = c(0), labels = c(expression(mu[0]))) +
  labs(x = "", y = "Density") + theme_bw() +
  ggtitle(expression(paste("Rejection region for alternative hypothesis: ", mu < mu[0]))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

# LOH
png(file = "nullTests4.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-3.5, 3.5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(-3.4, -1.96), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000) + 
  geom_segment(aes(x = -1.96, xend = -1.96, y = 0, yend = dnorm(-1.96))) +
  scale_x_continuous(breaks = c(0), labels = c(expression(mu[0]))) +
  annotate("point", x = -2.2, y = 0, color = "red", size = 2) +
  labs(x = "", y = "Density") + theme_bw() +
  ggtitle(expression(paste("Rejection region for alternative hypothesis: ", mu < mu[0]))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

########### Student's t-distribution ############
set.seed(33333)
df4 <- rbind(data.frame(x = seq(-6, 6, .001), y = dnorm(seq(-6, 6, .001), mean = 0, sd = 1/sqrt(3)), Distribution = "Normal"),
             data.frame(x = seq(-6, 6, .001), y = dt(seq(-6, 6, .001), df = 2), Distribution = "t (2 df)"),
             data.frame(x = seq(-6, 6, .001), y = dt(seq(-6, 6, .001), df = 9), Distribution = "t (9 df)"))

ggplot(df4, aes(x = x, y = y, group = Distribution, color = Distribution)) + geom_line() + theme_bw()

set.seed(333333)
df5 <- data.frame(x = replicate(100000, mean(rnorm(3))))

ggplot(df4, aes(x = x, y = ..density..)) + geom_histogram(bins = 60, fill = "grey65", color = "black") + 
  geom_line(data = df3 %>% filter(Distribution == "Normal"), aes(x = x, y = y)) + xlim(-1, 6) +
  theme_bw()
