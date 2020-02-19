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

# or 1,000 or 10,000 test statistics
set.seed(1111111)
df3b <- data.frame(x = replicate(100000, mean(rnorm(10))), y = runif(100000, 0, .07))
df3b$Test <- "Null is true"

png(file = "rare1.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df4, aes(x = x, y = y)) + geom_line(lwd = 1) + 
  geom_point(data = df3b[1:100,], aes(x = x, y = y, color = Test), size = .5) + 
  scale_x_continuous(breaks = c(0), labels = c(expression(mu[0]))) +
  annotate("point", x = -1.4, y = 0.0175, color = "blue", size = 1.25) + 
  annotate("label", x = -1.6, y = 0.15, label = "Observed TS") + 
  labs(x = "", y = "Density", color = "Test Statistic") + theme_bw() +
  ggtitle(expression(paste("TS's from 100 samples given the null hypothesis:", mu > mu[0])),
          "0/100 of the 'null is true TS' are as extreme as the Observed TS") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
dev.off()

png(file = "rare2.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df4, aes(x = x, y = y)) + geom_line(lwd = 1) + 
  geom_point(data = df3b[1:1000,], aes(x = x, y = y, color = Test), size = .5) + 
  scale_x_continuous(breaks = c(0), labels = c(expression(mu[0]))) +
  annotate("point", x = -1.4, y = 0.0175, color = "blue", size = 1.25) + 
  annotate("label", x = -1.6, y = 0.15, label = "Observed TS") + 
  labs(x = "", y = "Density", color = "Test Statistic") + theme_bw() +
  ggtitle(expression(paste("TS's from 1000 samples given the null hypothesis:", mu > mu[0])),
          "0/1000 of the 'null is true TS' are as extreme as the Observed TS") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
dev.off()

png(file = "rare3.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df4, aes(x = x, y = y)) + geom_line(lwd = 1) + 
  geom_point(data = df3b[1:10000,], aes(x = x, y = y, color = Test), size = .5) + 
  scale_x_continuous(breaks = c(0), labels = c(expression(mu[0]))) +
  annotate("point", x = -1.4, y = 0.0175, color = "blue", size = 1.25) + 
  annotate("label", x = -1.6, y = 0.15, label = "Observed TS") + 
  labs(x = "", y = "Density", color = "Test Statistic") + theme_bw() +
  ggtitle(expression(paste("TS's from 1000 samples given the null hypothesis:", mu > mu[0])),
          "1/10000 of the 'null is true TS' is as extreme as the Observed TS") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) 
dev.off()

########### Rejection regions ############
png(file = "nullTests3.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-3.5, 3.5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(-3.4, qnorm(.05)), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000) + 
  geom_segment(aes(x = qnorm(.05), xend = qnorm(.05), y = 0, yend = dnorm(qnorm(.05)))) +
  scale_x_continuous(breaks = c(0), labels = c(expression(mu[0]))) +
  labs(x = "", y = "Density") + theme_bw() +
  ggtitle(expression(paste("Rejection region for alternative hypothesis: ", mu < mu[0]))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

png(file = "nullTests3Flip.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-3.5, 3.5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(qnorm(.95), 3.4), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000) + 
  geom_segment(aes(x = qnorm(.95), xend = qnorm(.95), y = 0, yend = dnorm(qnorm(.95)))) +
  scale_x_continuous(breaks = c(0), labels = c(expression(mu[0]))) +
  labs(x = "", y = "Density") + theme_bw() +
  ggtitle(expression(paste("Rejection region for alternative hypothesis: ", mu > mu[0]))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

png(file = "nullTests4.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-3.5, 3.5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(-3.4, qnorm(.05)), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000) + 
  geom_segment(aes(x = qnorm(.05), xend = qnorm(.05), y = 0, yend = dnorm(qnorm(.05)))) +
  scale_x_continuous(breaks = c(-2.5, 0), labels = c(expression(bar(y)), expression(mu[0]))) +
  annotate("point", x = -2.5, y = 0, color = "red", size = 3) +
  annotate("label", x = -2.75, y = .1, label = "Test statistic is in RR") +
  labs(x = "", y = "Density") + theme_bw() +
  ggtitle(expression(paste("Rejection region for alternative hypothesis: ", mu < mu[0]))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

png(file = "nullTests5.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-3.5, 3.5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(-3.4, qnorm(.05)), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000) + 
  geom_segment(aes(x = qnorm(.05), xend = qnorm(.05), y = 0, yend = dnorm(qnorm(.05)))) +
  scale_x_continuous(breaks = c(-.5, 0), labels = c(expression(bar(y)), expression(mu[0]))) +
  annotate("point", x = -.5, y = 0, color = "red", size = 3) +
  annotate("label", x = -.5, y = .1, label = "Test statistic is not in RR") +
  labs(x = "", y = "Density") + theme_bw() +
  ggtitle(expression(paste("Rejection region for alternative hypothesis: ", mu < mu[0]))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

png(file = "nullTests6.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-3.5, 3.5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(-3.4, qnorm(.025)), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x), xlim = c(qnorm(.975), 3.4), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000) + 
  geom_segment(aes(x = qnorm(.025), xend = qnorm(.025), y = 0, yend = dnorm(qnorm(.025)))) +
  geom_segment(aes(x = qnorm(.975), xend = qnorm(.975), y = 0, yend = dnorm(qnorm(.975)))) +
  scale_x_continuous(breaks = c(0), labels = c(expression(mu[0]))) +
  labs(x = "", y = "Density") + theme_bw() +
  ggtitle(expression(paste("Rejection region for alternative hypothesis: ", mu == mu[0]))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

# SBP example:
png(file = "SBP1.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-3.5, 3.5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(-3.4, qnorm(.05)), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000) + 
  geom_segment(aes(x = qnorm(.05), xend = qnorm(.05), y = 0, yend = dnorm(qnorm(.05)))) +
  scale_x_continuous(breaks = c(-1.645, 0, 2.828), labels = c(expression(z = -1.645), 0, 2.828)) +
  annotate("point", x = 2.828, y = 0, color = "red", size = 3) +
  annotate("label", x = 2.828, y = .025, label = "Test statistic") +
  labs(x = "", y = "Density") + theme_bw() +
  ggtitle(expression(paste("Rejection region for alternative hypothesis: ", mu < mu[0]))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

########### Significance level ############
png(file = "sig1.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-3.5, 4.5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(2, 4.5), geom = "area", fill = "magenta", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = .5, n = 50000) +
  scale_x_continuous(breaks = c(0, 2), labels = c(0, "TS")) +
  annotate("label", x = 3, y = .075, label = expression(paste("p-value is: ", P(Z >= TS)))) +
  labs(x = "", y = "Density") + theme_bw() +
  ggtitle(expression(paste("Alternative hypothesis: ", mu > mu[0]))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

png(file = "sig2.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-4.5, 4.5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(2, 4.5), geom = "area", fill = "magenta", n = 10000) +
  stat_function(fun = function(x) dnorm(x), xlim = c(-4.5, -2), geom = "area", fill = "magenta", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = .5, n = 50000) +
  scale_x_continuous(breaks = c(-2, 0, 2), labels = c("-|TS|", 0, "|TS|")) +
  annotate("label", x = 3, y = .075, label = expression(P(Z >= TS))) +
  annotate("label", x = -3, y = .075, label = expression(P(Z <= TS))) +
  labs(x = "", y = "Density") + theme_bw() +
  ggtitle(expression(paste("Alternative hypothesis: ", mu == mu[0]))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

# SBP example:
round(pnorm(2.828, lower.tail = FALSE), 3)

########### Prostate cancer and cell cycle ############
stagec <- rpart::stagec
stagec <- stagec[!is.na(stagec$g2),]

(mean(stagec$g2) - (1/6 * 100)) / (sd(stagec$g2) / sqrt(length(stagec$g2)))

########### Student's t-distribution ############
df6 <- data.frame(x = seq(-5, 5, .001))
df6$Normal <- dnorm(df6$x)
df6$t1 <- dt(df6$x, 1) 
df6$t2 <- dt(df6$x, 2)
df6$t5 <- dt(df6$x, 5)
df6 <- df6 %>% gather(key = "Distribution", value = "value", -x)
df6$Distribution <- factor(df6$Distribution, levels = c("t1", "t2", "t5", "Normal"))
levels(df6$Distribution) <- c("t (df = 1)", "t (df = 2)", "t (df = 5)", "Normal")

png(file = "tDists.png", height = 5, width = 7, units = "in", res = 600)
ggplot(df6, aes(x = x, y = value, group = Distribution, color = Distribution)) + geom_line() + theme_bw() +
  labs(x = "", y = "Density") + ggtitle("Some Student's t-distributions") +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

########### Renal Cell carcinoma example ############
set.seed(3)
x <- rnorm(8, mean = 5.2)
x <- round(x, 3)
x
mean(x)
sd(x)

t.test(x, mu = 3.34)
