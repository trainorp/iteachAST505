############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture18_LinearRegression"))

########### Simulated regression lines ###########
set.seed(3)
df1 <- data.frame(x = runif(80, -1, 4))
set.seed(33)
df1$y <- 1 + .75 * df1$x + rnorm(80, 0, 0.5)

png(file = "p1.png", height = 5, width = 7, units = "in", res = 600)
ggplot(df1, aes(x, y)) + geom_point() + geom_vline(xintercept = 0, color = "grey30") + geom_hline(yintercept = 0, color = "grey30") +
  geom_abline(slope = .75, intercept = 1, color = "darkblue", lwd = 1.25) + xlim(-1, 4.1) + ylim(-.5, 5) + theme_minimal()
dev.off()

set.seed(3)
df1 <- data.frame(x = runif(80, -1, 4))
set.seed(33)
df1$y <- 1 + .75 * df1$x + rnorm(80, 0, 1)

png(file = "p2.png", height = 5, width = 7, units = "in", res = 600)
ggplot(df1, aes(x, y)) + geom_point() + geom_vline(xintercept = 0, color = "grey30") + geom_hline(yintercept = 0, color = "grey30") +
  geom_abline(slope = .75, intercept = 1, color = "darkblue", lwd = 1.25) + xlim(-1, 4.1) + ylim(-.5, 5) + theme_minimal()
dev.off()


set.seed(3)
df1 <- data.frame(x = runif(80, -1, 4))
set.seed(33)
df1$y <- 1 + .75 * df1$x + rnorm(80, 0, .25)

png(file = "p3.png", height = 5, width = 7, units = "in", res = 600)
ggplot(df1, aes(x, y)) + geom_point() + geom_vline(xintercept = 0, color = "grey30") + geom_hline(yintercept = 0, color = "grey30") +
  geom_abline(slope = .75, intercept = 1, color = "darkblue", lwd = 1.25) + xlim(-1, 4.1) + ylim(-.5, 5) + theme_minimal()
dev.off()

########### violated assumptions ###########
df0 <- df1

# Non-linear
df1$y <- (df1$y - 2)^2
png(file = "p4.png", height = 5, width = 7, units = "in", res = 600)
ggplot(df1, aes(x, y)) + geom_point() + geom_vline(xintercept = 0, color = "grey30") + geom_hline(yintercept = 0, color = "grey30") +
   theme_minimal()
dev.off()

# Expected value not zero
df1 <- df0
df1$y[df1$x < 1.75] <- df1$y[df1$x < 1.75]- 1
df1$y[df1$x > 1.75]<- df1$y[df1$x > 1.75] + 1
png(file = "p5.png", height = 5, width = 7, units = "in", res = 600)
ggplot(df1, aes(x, y)) + geom_point() + geom_vline(xintercept = 0, color = "grey30") + geom_hline(yintercept = 0, color = "grey30") +
  geom_abline(slope = .75, intercept = 1, color = "darkblue", lwd = 1.25) + xlim(-1, 4.1) + ylim(-.5, 5) + theme_minimal()
dev.off()

# Non-constant variance: 
set.seed(3)
df1 <- data.frame(x = runif(80, -1, 4))
df1 <- df1 %>% arrange(x)
df1$vars <- seq(.1, 1.25, length.out = 80)^1.5
df1$err <- NA
set.seed(333)
for(i in 1:nrow(df1)) df1$err[i] <- rnorm(1, 0, df1$vars[i])
df1$y <- 1 + .75 * df1$x + df1$err

png(file = "p6.png", height = 5, width = 7, units = "in", res = 600)
ggplot(df1, aes(x, y)) + geom_point() + geom_vline(xintercept = 0, color = "grey30") + geom_hline(yintercept = 0, color = "grey30") +
  geom_abline(slope = .75, intercept = 1, color = "darkblue", lwd = 1.25) + xlim(-1, 4.1) + ylim(-.5, 5) + theme_minimal()
dev.off()

# Errors not independent: 
set.seed(3)
df1 <- data.frame(x = runif(80, -1, 4))
df1 <- df1 %>% arrange(x)
df1$errMean <- seq(0, 4*pi, length.out = 80)
df1$errMean <- sin(df1$errMean)
df1$err <- NA
set.seed(333)
for(i in 1:nrow(df1)) df1$err[i] <- rnorm(1, df1$errMean[i], .2)
df1$y <- 1 + .75 * df1$x + df1$err

png(file = "p7.png", height = 5, width = 7, units = "in", res = 600)
ggplot(df1, aes(x, y)) + geom_point() + geom_vline(xintercept = 0, color = "grey30") + geom_hline(yintercept = 0, color = "grey30") +
  geom_abline(slope = .75, intercept = 1, color = "darkblue", lwd = 1.25) + xlim(-1, 4.1) + ylim(-.5, 5) + theme_minimal()
dev.off()

########### Small dataset ###########
set.seed(33333)
df1 <- data.frame(Cortisol = runif(8, 10, 17))
set.seed(3333)
df1$SBP <-  + 120 + 2*(df1$Cortisol - mean(df1$Cortisol)) + rnorm(8, 0, 4)
df1$Cortisol <- round(df1$Cortisol, 2)
df1$SBP <- round(df1$SBP, 2)

lm1 <- lm(SBP ~ Cortisol, data = df1)
summary(lm1)

xtable::xtable(df1)

png(file = "p8.png", height = 5, width = 6.5, units = "in", res = 600)
ggplot(df1, aes(x = Cortisol, y = SBP)) + geom_point() + theme_bw()
dev.off()

png(file = "p9.png", height = 5, width = 6.5, units = "in", res = 600)
ggplot(df1, aes(x = Cortisol, y = SBP)) + geom_point() + 
  geom_abline(slope = 2.0875, intercept = 92.3177, color = "darkblue", lwd = 1.25) + labs(x = "Cortisol (x)", y = "SBP (y)") + 
  theme_bw()
dev.off()

png(file = "p9b.png", height = 5, width = 6.5, units = "in", res = 600)
ggplot(df1, aes(x = Cortisol, y = SBP)) + geom_point() + 
  geom_abline(slope = 2.0875, intercept = 92.3177, color = "darkblue", lwd = 1.25) + labs(x = "Cortisol (x)", y = "SBP (y)") + 
  annotate("label", x = 14.5, y = 112.5, label = expression(SBP == 92.3177 + 2.0875 * Cortisol), size = 7) +
  theme_bw()
dev.off()

png(file = "p9Beta0.png", height = 5, width = 6.5, units = "in", res = 600)
ggplot(df1, aes(x = Cortisol, y = SBP)) + geom_point() + 
  geom_abline(slope = 2.0875, intercept = 92.3177, color = "darkblue", lwd = 1.25) + labs(x = "Cortisol (x)", y = "SBP (y)") + 
  xlim(0, 20) + ylim(0, 200) + annotate("label", x = 10, y = 50, label = expression(SBP == 92.3177 + 2.0875 * Cortisol), size = 7) +
  theme_bw()
dev.off()

#### Parameter extimation ####
df1$Cortisol - mean(df1$Cortisol)
df1$SBP - mean(df1$SBP)

(df1$Cortisol - mean(df1$Cortisol)) * (df1$SBP - mean(df1$SBP))
sum((df1$Cortisol - mean(df1$Cortisol)) * (df1$SBP - mean(df1$SBP)))
Sxy <- sum((df1$Cortisol - mean(df1$Cortisol)) * (df1$SBP - mean(df1$SBP)))
(df1$Cortisol - mean(df1$Cortisol))^2
sum((df1$Cortisol - mean(df1$Cortisol))^2)
Sxx <- sum((df1$Cortisol - mean(df1$Cortisol))^2)

Sxy / Sxx

119.81 - (2.0875)*(13.17)

predict(lm1)
round(residuals(lm1), 3)
round(sum(round(residuals(lm1), 3)^2), 3)
round(sum(round(residuals(lm1), 3)^2), 3) / 6
sqrt(round(sum(round(residuals(lm1), 3)^2), 3) / 6)

3.560 * sqrt(1 / 36.1084)
3.560 * 1 / sqrt(36.1084)

2.0875 / 0.5924

2 * pt(2.0875 / 0.5924, 6, lower.tail = FALSE)

2.0875 - qt(.975, 6) * 0.592442
2.0875 + qt(.975, 6) * 0.592442
confint(lm1)

mean(df1$Cortisol)^2
92.3177 - qt(.975, 6) * 3.56 * sqrt((1 / 8) + mean(df1$Cortisol)^2 / Sxx)
92.3177 + qt(.975, 6) * 3.56 * sqrt((1 / 8) + mean(df1$Cortisol)^2 / Sxx)

influence.measures(lm1)

df1b <- rbind(cbind(df1, group = "old"), data.frame(Cortisol = 22, SBP = NA, group = "new"))
set.seed(3333)
df1b$SBP[9] <- 120 + 2*(22 - mean(df1$Cortisol)) + .5 * rnorm(1, 0, 4)
summary(lm1)
lm2 <- update(lm1, data = df1b)
summary(lm2)

png(file = "p10.png", height = 5, width = 7, units = "in", res = 600)
ggplot(df1b, aes(Cortisol, SBP, color = group)) + geom_point() + 
  geom_abline(intercept = 92.3177, slope = 2.0875, color = "darkblue") + 
  geom_abline(intercept = 91.6814, slope = 2.1378, color = "indianred") + 
  labs(x = "Cortisol (x)", y = "SBP (y)")  + theme_bw() + guides(color = FALSE) + ylim(c(102, 145))
dev.off()

set.seed(3333)
df1b$SBP[9] <- 120 + 2*(22 - mean(df1$Cortisol)) + .5 * rnorm(1, 0, 4) - 14
lm2 <- update(lm1, data = df1b)
summary(lm2)

png(file = "p11.png", height = 5, width = 7, units = "in", res = 600)
ggplot(df1b, aes(Cortisol, SBP, color = group)) + geom_point() + 
  geom_abline(intercept = 92.3177, slope = 2.0875, color = "darkblue") + 
  geom_abline(intercept = 104.8771 , slope = 1.0954, color = "indianred") + 
  labs(x = "Cortisol (x)", y = "SBP (y)")  + theme_bw() + guides(color = FALSE)  + ylim(c(102, 145))
dev.off()

set.seed(3333)
df1b$SBP[9] <- 120 + 2*(22 - mean(df1$Cortisol)) + .5 * rnorm(1, 0, 4) - 34
lm2 <- update(lm1, data = df1b)
summary(lm2)

png(file = "p12.png", height = 5, width = 7, units = "in", res = 600)
ggplot(df1b, aes(Cortisol, SBP, color = group)) + geom_point() + 
  geom_abline(intercept = 92.3177, slope = 2.0875, color = "darkblue") + 
  geom_abline(intercept = 123.7280 , slope = -0.3938, color = "indianred") + 
  labs(x = "Cortisol (x)", y = "SBP (y)")  + theme_bw() + guides(color = FALSE)  + ylim(c(102, 145))
dev.off()

anova(lm1)

predict(lm1, newdata = data.frame(Cortisol = 15))

92.3177+2.0875*15

round(qt(.975, 6), 3)
92.3177+2.0875*15 - 2.447 * 3.560 * sqrt(1/8 + (15 - 13.17)^2 / 36.1084)
92.3177+2.0875*15 + 2.447 * 3.560 * sqrt(1/8 + (15 - 13.17)^2 / 36.1084)

predict(lm1, newdata = data.frame(Cortisol = 15), se.fit = TRUE, interval = "confidence")
predict(lm1, newdata = data.frame(Cortisol = 15), interval = "prediction")

png(file = "p9Conf.png", height = 5, width = 6.5, units = "in", res = 600)
ggplot(df1, aes(x = Cortisol, y = SBP)) + geom_point() + 
  geom_abline(slope = 2.0875, intercept = 92.3177, color = "darkblue", lwd = 1.25) + labs(x = "Cortisol (x)", y = "SBP (y)") + 
  geom_errorbar(mapping = aes(x = 15, ymin = 119.5652, ymax = 127.6952), color = "deeppink", lwd = 1.5, width = .5) +
  annotate("point", x = 15, y = 123.6301, color = "cyan", size = 8) + 
  annotate("label", x = 14.5, y = 112.5, label = expression(SBP == 92.3177 + 2.0875 * Cortisol), size = 7) +
  theme_bw()
dev.off()

png(file = "p9Confb.png", height = 5, width = 6.5, units = "in", res = 600)
ggplot(df1, aes(x = Cortisol, y = SBP)) + geom_point() + 
  geom_abline(slope = 2.0875, intercept = 92.3177, color = "darkblue", lwd = 1.25) + labs(x = "Cortisol (x)", y = "SBP (y)") + 
  geom_vline(xintercept = mean(df1$Cortisol), lty = 2, color = "orange") + 
  geom_errorbar(mapping = aes(x = 15, ymin = 119.5652, ymax = 127.6952), color = "deeppink", lwd = 1.5, width = .5) +
  annotate("point", x = 15, y = 123.6301, color = "cyan") + 
  xlim(0, 20) + ylim(0, 200) +
  theme_bw()
dev.off()

predict(lm1, newdata = data.frame(Cortisol = 2), se.fit = TRUE, interval = "confidence")
png(file = "p9Confc.png", height = 5, width = 6.5, units = "in", res = 600)
ggplot(df1, aes(x = Cortisol, y = SBP)) + geom_point() + 
  geom_abline(slope = 2.0875, intercept = 92.3177, color = "darkblue", lwd = 1.25) + labs(x = "Cortisol (x)", y = "SBP (y)") + 
  geom_vline(xintercept = mean(df1$Cortisol), lty = 2, color = "orange") + 
  geom_errorbar(mapping = aes(x = 2, ymin = 80.01065, ymax = 112.9748), color = "deeppink", lwd = 1.5, width = .5) +
  annotate("point", x = 2, y = 96.49271, color = "cyan") + 
  xlim(0, 20) + ylim(0, 200) +
  theme_bw()
dev.off()

# Prediction interval:
92.3177+2.0875*15 - 2.447 * 3.560 * sqrt(1 + 1/8 + (15 - 13.17)^2 / 36.1084)
92.3177+2.0875*15 + 2.447 * 3.560 * sqrt(1 + 1/8 + (15 - 13.17)^2 / 36.1084)
predict(lm1, newdata = data.frame(Cortisol = 15), interval = "prediction")

########### Bad residuals ###########
set.seed(9)
df2 <- data.frame(x = runif(50, 0, 10))
set.seed(9+3)
df2$y <- - df2$x + rnorm(50, 0, 1) 
lm2 <- lm(y ~ x, data = df2)

png(file = "p10.png", height = 5, width = 6.5, units = "in", res = 600)
ggplot(df2, aes(x = x, y = y)) + geom_point() + geom_abline(slope = coef(lm2)[2], intercept = coef(lm2)[1], color = "darkblue", lwd = 1.25) + 
  theme_bw()
dev.off()
df2$Predicted <- predict(lm2)
df2$Residuals <- lm2$residuals
png(file = "p11.png", height = 5, width = 6.5, units = "in", res = 600)
ggplot(df2, aes(x = Predicted, y = Residuals)) + geom_point() + 
  geom_hline(yintercept = 0, color = "darkred", lwd = 1.25) + 
  theme_bw()
dev.off()

set.seed(9+3)
df2$y <- (df2$x - 4) ^ 2 + rnorm(50, 0, 1) 
lm2 <- lm(y ~ x, data = df2)

png(file = "p10a.png", height = 5, width = 6.5, units = "in", res = 600)
ggplot(df2, aes(x = x, y = y)) + geom_point() + geom_abline(slope = coef(lm2)[2], intercept = coef(lm2)[1], color = "darkblue", lwd = 1.25) + 
  theme_bw()
dev.off()
df2$Predicted <- predict(lm2)
df2$Residuals <- lm2$residuals
png(file = "p11a.png", height = 5, width = 6.5, units = "in", res = 600)
ggplot(df2, aes(x = Predicted, y = Residuals)) + geom_point() + 
  geom_hline(yintercept = 0, color = "darkred", lwd = 1.25) + 
  theme_bw()
dev.off()

set.seed(9+3233)
df2$e <- rnorm(50, 0, .2)
df2$y <- df2$x + df2$e * (df2$x) ^ 1.5
lm2 <- lm(y ~ x, data = df2)

png(file = "p10b.png", height = 5, width = 6.5, units = "in", res = 600)
ggplot(df2, aes(x = x, y = y)) + geom_point() + geom_abline(slope = coef(lm2)[2], intercept = coef(lm2)[1], color = "darkblue", lwd = 1.25) + 
  theme_bw()
dev.off()
df2$Predicted <- predict(lm2)
df2$Residuals <- lm2$residuals
png(file = "p11b.png", height = 5, width = 6.5, units = "in", res = 600)
ggplot(df2, aes(x = Predicted, y = Residuals)) + geom_point() + 
  geom_hline(yintercept = 0, color = "darkred", lwd = 1.25) + 
  theme_bw()
dev.off()
