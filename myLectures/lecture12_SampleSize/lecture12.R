############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)
library(ggrepel)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture12_SampleSize"))

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
p1 <- ggplot(df2, aes()) + geom_segment(aes(x = x, xend = x, y = lower, yend = upper, color = contains)) +
  geom_point(aes(x = x, y = xbar, color = contains)) + geom_hline(yintercept = 0, color = "darkblue", lwd = 1.1) +
  scale_y_continuous(breaks = c(-1, 0, 1), labels = c(expression(mu - sigma), expression(mu), expression(mu + sigma)),
                     limits = c(-1.1, 1.1)) +
  theme_bw() + labs(x = "Random Sample #", y = "", color = expression(paste("Contains ", mu))) +
  annotate("label", x = 50, y = 1, label = "95 / 100 = 95%") +
  ggtitle(expression(paste("Confidence intervals from 100 random samples (n = 25) with ", 1-alpha == .95))) +
  theme(plot.title = element_text(hjust = 0.5)) 
show(p1)
dev.off()

png(file = "coverage1bb.png", height = 4, width = 6.5, units = "in", res = 600)
set.seed(3333)
xbar <- replicate(100, mean(sample(df1$x, 10)))
lower <- xbar - qnorm(.975) * 1 / sqrt(10)
upper <- xbar + qnorm(.975) * 1 / sqrt(10)
df2 <- data.frame(xbar = xbar, lower = lower, upper = upper, x = 1:length(xbar))
df2$contains <- ifelse(df2$lower < 0 & df2$upper > 0, "Yes", "No")
p1bb <- ggplot(df2, aes()) + geom_segment(aes(x = x, xend = x, y = lower, yend = upper, color = contains)) +
  geom_point(aes(x = x, y = xbar, color = contains)) + geom_hline(yintercept = 0, color = "darkblue", lwd = 1.1) +
  scale_y_continuous(breaks = c(-1, 0, 1), labels = c(expression(mu - sigma), expression(mu), expression(mu + sigma)),
                     limits = c(-1.5, 1.55)) +
  theme_bw() + labs(x = "Random Sample #", y = "", color = expression(paste("Contains ", mu))) +
  annotate("label", x = 50, y = 1.2, label = "94 / 100 = 94%") +
  ggtitle(expression(paste("Confidence intervals from 100 random samples (n = 10) with ", 1-alpha == .95))) +
  theme(plot.title = element_text(hjust = 0.5)) 
show(p1bb)
dev.off()

png(file = "coverage1bbb.png", height = 4, width = 6.5, units = "in", res = 600)
set.seed(333)
xbar <- replicate(100, mean(sample(df1$x, 100)))
lower <- xbar - qnorm(.975) * 1 / sqrt(100)
upper <- xbar + qnorm(.975) * 1 / sqrt(100)
df2 <- data.frame(xbar = xbar, lower = lower, upper = upper, x = 1:length(xbar))
df2$contains <- ifelse(df2$lower < 0 & df2$upper > 0, "Yes", "No")
p1bbb <- ggplot(df2, aes()) + geom_segment(aes(x = x, xend = x, y = lower, yend = upper, color = contains)) +
  geom_point(aes(x = x, y = xbar, color = contains)) + geom_hline(yintercept = 0, color = "darkblue", lwd = 1.1) +
  scale_y_continuous(breaks = c(-1, 0, 1), labels = c(expression(mu - sigma), expression(mu), expression(mu + sigma)),
                     limits = c(-1.5, 1.55)) +
  theme_bw() + labs(x = "Random Sample #", y = "", color = expression(paste("Contains ", mu))) +
  annotate("label", x = 50, y = 1.2, label = "95 / 100 = 95%") +
  ggtitle(expression(paste("Confidence intervals from 100 random samples (n = 100) with ", 1-alpha == .95))) +
  theme(plot.title = element_text(hjust = 0.5)) 
show(p1bbb)
dev.off()

png(file = "coverage1b.png", height = 4, width = 6.5, units = "in", res = 600)
set.seed(3333)
xbar <- replicate(100, mean(sample(df1$x, 25)))
lower <- xbar - qnorm(.9) * 1 / sqrt(25)
upper <- xbar + qnorm(.9) * 1 / sqrt(25)
df2 <- data.frame(xbar = xbar, lower = lower, upper = upper, x = 1:length(xbar))
df2$contains <- ifelse(df2$lower < 0 & df2$upper > 0, "Yes", "No")
p1b <- ggplot(df2, aes()) + geom_segment(aes(x = x, xend = x, y = lower, yend = upper, color = contains)) +
  geom_point(aes(x = x, y = xbar, color = contains)) + geom_hline(yintercept = 0, color = "darkblue", lwd = 1.1) +
  scale_y_continuous(breaks = c(-1, 0, 1), labels = c(expression(mu - sigma), expression(mu), expression(mu + sigma)),
                     limits = c(-1.1, 1.1)) +
  theme_bw() + labs(x = "Random Sample #", y = "", color = expression(paste("Contains ", mu))) +
  annotate("label", x = 50, y = 1, label = "80 / 100 = 80%") +
  ggtitle(expression(paste("Confidence intervals from 100 random samples (n = 25) with ", 1-alpha == .80))) +
  theme(plot.title = element_text(hjust = 0.5)) 
show(p1b)
dev.off()

png(file = "coverage1c.png", height = 8, width = 9, units = "in", res = 600)
gridExtra::grid.arrange(p1, p1b)
dev.off()

png(file = "coverage1cc.png", height = 8, width = 9, units = "in", res = 600)
gridExtra::grid.arrange(p1bb, p1bbb)
dev.off()

png(file = "coverage2.png", height = 4, width = 6.5, units = "in", res = 600)
set.seed(2222)
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
  annotate("label", x = 50, y = 1, label = "94 / 100 = 94%") +
  ggtitle(expression(paste("Confidence intervals from 100 random samples (n = 25) with ", 1-alpha == .95))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

png(file = "coverage3.png", height = 4, width = 6.5, units = "in", res = 600)
set.seed(3)
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
  ggtitle(expression(paste("Confidence intervals from 100 random samples (n = 25) with ", 1-alpha == .95))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

