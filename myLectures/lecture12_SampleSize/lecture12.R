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

############ Water example ############
set.seed(333)
w1 <- data.frame(lead = rnorm(n = 10, mean = 4.5, sd = 1))
w1$lead <- round(w1$lead, 2)
w1Mean <- mean(w1$lead)
w1z <- (w1Mean - 5)/(1 / sqrt(length(w1$lead)))

png(file = "rr.png", height = 4, width = 6, units = "in", res = 600)
ggplot(data.frame(x = c(-3.5, 3.5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), xlim = c(-3.5, qnorm(.01)), geom = "area", fill = "cyan", n = 10000) +
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 10000) + 
  geom_segment(aes(x = qnorm(.01), xend = qnorm(.01), y = 0, yend = dnorm(qnorm(.01)))) +
  scale_x_continuous(breaks = c(w1z, 0), labels = c("z*", 0)) +
  annotate("point", x = w1z, y = 0, color = "red", size = 3) +
  annotate("label", x = w1z, y = .1, label = "Test statistic is not in RR") +
  labs(x = "", y = "Density") + theme_bw() +
  ggtitle(expression(paste("Rejection region for alternative hypothesis: ", mu < mu[0]))) +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

w1z2 <- (w1Mean - 5) / (1 / sqrt(100))
pnorm(w1z2)


############ Overlapping histograms ############
df5 <- data.frame(x = seq(0, 10, .001), Distribution = "Null")
df5$Density <- dnorm(df5$x, mean = 5, sd = 1)
df6 <- data.frame(x = seq(4.5 - 5, 4.5 + 5, .001), Distribution = "Specific Alternative\nmu=4")
df6$Density <- dnorm(df5$x, mean = 4.5, sd = 1)
df5 <- rbind(df5, df6)
png(file = "overlap0.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df5, aes(x = x, y = Density, group = Distribution, color = Distribution)) + geom_line(lwd = 1.25) + theme_bw() +
  labs(x = "Lead (ppb)") + ggtitle("Population distributions") + theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

df5 <- data.frame(x = seq(0, 10, .001), Distribution = "Null")
df5$Density <- dnorm(df5$x, mean = 5, sd = 2 / sqrt(5))
df6 <- data.frame(x = seq(4.5 - 5, 4.5 + 5, .001), Distribution = "Specific Alternative\nmu=4")
df6$Density <- dnorm(df5$x, mean = 4.5, sd = 2 / sqrt(5))
df5 <- rbind(df5, df6)
png(file = "overlap1.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df5, aes(x = x, y = Density, group = Distribution, color = Distribution)) + geom_line(lwd = 1.25) +
  geom_area(data = df5 %>% filter(Distribution == "Specific Alternative\nmu=4" & x > 3), aes(x = x, y = Density, color = NULL),
            fill = rgb(0, 90, 100, 0, alpha = 70, maxColorValue = 255), show.legend = FALSE) +
  theme_bw() + annotate("label", x = 4.25, y = .03, label = expression(beta)) +
  labs(x = "Lead (ppb)") + ggtitle("Sampling distributions for the sample mean (n = 5)") + theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

png(file = "overlap11.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df5, aes(x = x, y = Density, group = Distribution, color = Distribution)) + geom_line(lwd = 1.25) +
  geom_area(data = df5 %>% filter(Distribution == "Specific Alternative\nmu=4" & x < 3), aes(x = x, y = Density, color = NULL),
            fill = rgb(0, 150, 0, 0, alpha = 70, maxColorValue = 255), show.legend = FALSE) +
  theme_bw() + annotate("label", x = 2, y = .03, label = expression(1-beta)) +
  labs(x = "Lead (ppb)") + ggtitle("Sampling distributions for the sample mean (n = 5)") + theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

df5 <- data.frame(x = seq(0, 10, .001), Distribution = "Null")
df5$Density <- dnorm(df5$x, mean = 5, sd = 2 / sqrt(10))
df6 <- data.frame(x = seq(4.5 - 5, 4.5 + 5, .001), Distribution = "Specific Alternative\nmu=4")
df6$Density <- dnorm(df5$x, mean = 4.5, sd = 2 / sqrt(10))
df5 <- rbind(df5, df6)
png(file = "overlap2.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df5, aes(x = x, y = Density, group = Distribution, color = Distribution)) + geom_line(lwd = 1.25) + 
  geom_area(data = df5 %>% filter(Distribution == "Specific Alternative\nmu=4" & x > 3.7), aes(x = x, y = Density, color = NULL),
            fill = rgb(0, 90, 100, 0, alpha = 70, maxColorValue = 255), show.legend = FALSE)+
  theme_bw() + annotate("label", x = 4.75, y = .03, label = expression(beta)) +
  labs(x = "Lead (ppb)") + ggtitle("Sampling distributions for the sample mean (n = 10)") + theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

df5 <- data.frame(x = seq(0, 10, .001), Distribution = "Null")
df5$Density <- dnorm(df5$x, mean = 5, sd = 2 / sqrt(25))
df6 <- data.frame(x = seq(4.5 - 5, 4.5 + 5, .001), Distribution = "Specific Alternative\nmu=4")
df6$Density <- dnorm(df5$x, mean = 4.5, sd = 2 / sqrt(25))
df5 <- rbind(df5, df6)
png(file = "overlap3.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df5, aes(x = x, y = Density, group = Distribution, color = Distribution)) + geom_line(lwd = 1.25) + 
  geom_area(data = df5 %>% filter(Distribution == "Specific Alternative\nmu=4" & x > 4.15), aes(x = x, y = Density, color = NULL),
            fill = rgb(0, 90, 100, 0, alpha = 70, maxColorValue = 255), show.legend = FALSE)+
  theme_bw() + annotate("label", x = 4.8, y = .03, label = expression(beta)) +
  labs(x = "Lead (ppb)") + ggtitle("Sampling distributions for the sample mean (n = 25)") + theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

df5 <- data.frame(x = seq(0, 10, .001), Distribution = "Null")
df5$Density <- dnorm(df5$x, mean = 5, sd = 1 / sqrt(100))
df6 <- data.frame(x = seq(4.5 - 5, 4.5 + 5, .001), Distribution = "Specific Alternative\nmu=4")
df6$Density <- dnorm(df5$x, mean = 4.5, sd = 1 / sqrt(100))
df5 <- rbind(df5, df6)
png(file = "overlap4.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df5, aes(x = x, y = Density, group = Distribution, color = Distribution)) + geom_line(lwd = 1.25) + theme_bw() +
  labs(x = "Lead (ppb)") + ggtitle("Sampling distributions for the sample mean (n = 100)") + theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

df5 <- data.frame(x = seq(0, 10, .001), Distribution = "Null")
df5$Density <- dnorm(df5$x, mean = 5, sd = 2 / sqrt(5))
df6 <- data.frame(x = seq(3 - 5, 3 + 5, .001), Distribution = "Specific Alternative\nmu=2.5")
df6$Density <- dnorm(df5$x, mean = 4.5, sd = 2 / sqrt(5))
df5 <- rbind(df5, df6)
png(file = "overlap1b.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df5, aes(x = x, y = Density, group = Distribution, color = Distribution)) + geom_line(lwd = 1.25) +
  geom_area(data = df5 %>% filter(Distribution == "Specific Alternative\nmu=2.5" & x > 3), aes(x = x, y = Density, color = NULL),
            fill = rgb(0, 90, 100, 0, alpha = 70, maxColorValue = 255), show.legend = FALSE) +
  theme_bw() + annotate("label", x = 3.75, y = .03, label = expression(beta)) +
  labs(x = "Lead (ppb)") + ggtitle("Sampling distributions for the sample mean (n = 5)") + theme(plot.title = element_text(hjust = 0.5)) 
dev.off()


df5 <- data.frame(x = seq(-5, 5, .001), Distribution = "Null")
df5$Density <- dnorm(df5$x, mean = 0, sd = 1 )
df6 <- data.frame(x = df5$x + (4.5-5)/(1/sqrt(10)), Density = df5$Density, Distribution = "Specific\nAlternative")
df5 <- rbind(df5, df6)
png(file = "overlapStandard.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df5, aes(x = x, y = Density, group = Distribution, color = Distribution)) + geom_line(lwd = 1.25) +
  geom_area(data = df5 %>% filter(Distribution == "Specific\nAlternative" & x > qnorm(.01)), aes(x = x, y = Density, color = NULL),
            fill = rgb(0, 90, 100, 0, alpha = 70, maxColorValue = 255), show.legend = FALSE) +
  geom_segment(aes(x = qnorm(.01), xend = qnorm(.01), y = 0, yend = dnorm(qnorm(.01))), lwd = 1.1, color = "black") +
  theme_bw() + annotate("label", x = -.75, y = .03, label = expression(beta)) +
  labs(x = "z") + ggtitle(expression(paste(beta, " from standard normal"))) + theme(plot.title = element_text(hjust = 0.5)) 
dev.off()

########### Beta curve ###########
bFun <- function(alpha, mu0, mua, sigma, n){
  return(pnorm(qnorm(alpha, lower.tail = FALSE) - (abs(mu0-mua)/(sigma / sqrt(n)))))
}

df1 <- data.frame(ua = seq(3, 5, .25))
df1$beta <- bFun(.01, 5, df1$ua, 1, 10)
df1$beta <- round(df1$beta, 3)
df1
df1$Power <- 1 - df1$beta
df1

png(file = "beta1.png", height = 4, width = 6, units = "in", res = 300)
ggplot(df1, aes(x = ua, y = beta)) + geom_point(size = 2) + theme_bw() + labs(x = expression(mu[a]), y = expression(beta))
dev.off()

df1 <- data.frame(ua = seq(3, 5, .01))
df1$beta <- bFun(.01, 5, df1$ua, 1, 10)

png(file = "beta2.png", height = 4, width = 6, units = "in", res = 300)
ggplot(df1, aes(x = ua, y = beta)) + geom_line(size = 1.25, color = "darkblue") + theme_bw() + labs(x = expression(mu[a]), y = expression(beta))
dev.off()

png(file = "beta3.png", height = 4, width = 6, units = "in", res = 300)
ggplot(df1, aes(x = ua, y = 1-beta)) + geom_line(size = 1.25, color = "darkred") + theme_bw() + 
  labs(x = expression(mu[a]), y = expression(1-beta))
dev.off()

########### Glasgow power ###########
20 / (100/sqrt(100))
qnorm(.95) - 20 / (100/sqrt(100))
pnorm(qnorm(.95) - 20 / (100/sqrt(100)))
pnorm(qnorm(.95) - 20 / (100/sqrt(1000)))

powerFun <- function(alpha, mu0, mua, sigma, n){
  return(1-pnorm(qnorm(alpha/2, lower.tail = FALSE) - (abs(mu0-mua)/(sigma / sqrt(n)))))
}

df1 <- expand.grid(alpha = c(0.01, 0.05, 0.10), n = c(25, 50, 100, 150, 200), sigma = 100, mu0 = 867, 
                   mua = seq(867-50, 867+50, .01))
df1$power <- NA
for(i in 1:nrow(df1)){
  df1$power[i] <- powerFun(df1$alpha[i], df1$mu0[i], df1$mua[i], df1$sigma[i], df1$n[i])
}

png(filename = "Power1.png", height = 4, width = 6, units = "in", res = 300)
ggplot(df1 %>% filter(alpha == 0.05), aes(x = mua, y = power, group = n, color = as.factor(n))) + geom_line(lwd = 1) + 
  theme_bw() + labs(x = expression(mu[a]), y = expression(1-beta), color = "Sample Size") + 
  scale_color_brewer(palette = "Set1") + ggtitle(expression(paste("Fixed ", alpha == 0.05, " various sample sizes"))) +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

png(filename = "Power2.png", height = 4, width = 6, units = "in", res = 300)
ggplot(df1 %>% filter(n == 50), aes(x = mua, y = power, group = alpha, color = as.factor(alpha))) + geom_line(lwd = 1) + 
  theme_bw() + labs(x = expression(mu[a]), y = expression(1-beta), color = expression(alpha)) + 
  scale_color_brewer(palette = "Set2") + ggtitle(expression(paste("Fixed ", n == 50, " various ", alpha))) +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
       