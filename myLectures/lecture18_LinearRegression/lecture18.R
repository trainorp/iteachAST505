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
lmIDK <- lm(y ~ x, data = df1)
png(file = "p5.png", height = 5, width = 7, units = "in", res = 600)
ggplot(df1, aes(x, y)) + geom_point() + geom_vline(xintercept = 0, color = "grey30") + geom_hline(yintercept = 0, color = "grey30") +
  geom_abline(slope = 1.2, intercept = 0.45, color = "darkblue", lwd = 1.25) + xlim(-1, 4.1) + ylim(-.5, 5) + theme_minimal()
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
write.csv(df1, file = "df1.csv", row.names = FALSE)

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
Syy <- sum((df1$SBP - mean(df1$SBP))^2)

Sxy / Sxx

# Correlation coefficient:
75.376 / sqrt(36.1084*233.3806)
(233.3806 - 76.034)/(233.3806)

119.81 - (2.0875)*(13.17)

0.8211 * sqrt(6) / sqrt(1-0.6742)
round(0.8211 * sqrt(6) / sqrt(1-0.6742), 3)
cor.test(df1$Cortisol, df1$SBP, alternative = "greater")
summary(lm1)
cor.test(df1$Cortisol, df1$SBP)
cor.test(df1$Cortisol, df1$SBP, conf.level = .9)


.5 * log((1+0.8211)/(1-0.8211))
z1 <- 1.160185 - (1.645)/sqrt(5) 
z2 <- 1.160185 + (1.645)/sqrt(5) 
(exp(2* z1)-1)/(exp(2*z1)+1)
(exp(2* z2)-1)/(exp(2*z2)+1)

# Intercept only model:
lm0 <- update(lm1, . ~ . - Cortisol)
anova(lm1, lm0)

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

png(file = "p0_10.png", height = 5, width = 7, units = "in", res = 600)
ggplot(df1b, aes(Cortisol, SBP, color = group)) + geom_point() + 
  geom_abline(intercept = 92.3177, slope = 2.0875, color = "darkblue") + 
  geom_abline(intercept = 91.6814, slope = 2.1378, color = "indianred") + 
  labs(x = "Cortisol (x)", y = "SBP (y)")  + theme_bw() + guides(color = FALSE) + ylim(c(102, 145))
dev.off()

set.seed(3333)
df1b$SBP[9] <- 120 + 2*(22 - mean(df1$Cortisol)) + .5 * rnorm(1, 0, 4) - 14
lm2 <- update(lm1, data = df1b)
summary(lm2)

png(file = "p0_11.png", height = 5, width = 7, units = "in", res = 600)
ggplot(df1b, aes(Cortisol, SBP, color = group)) + geom_point() + 
  geom_abline(intercept = 92.3177, slope = 2.0875, color = "darkblue") + 
  geom_abline(intercept = 104.8771 , slope = 1.0954, color = "indianred") + 
  labs(x = "Cortisol (x)", y = "SBP (y)")  + theme_bw() + guides(color = FALSE)  + ylim(c(102, 145))
dev.off()

set.seed(3333)
df1b$SBP[9] <- 120 + 2*(22 - mean(df1$Cortisol)) + .5 * rnorm(1, 0, 4) - 34
lm2 <- update(lm1, data = df1b)
summary(lm2)

png(file = "p0_12.png", height = 5, width = 7, units = "in", res = 600)
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

########### Heterogenous variance example ###########
set.seed(5)
df3 <- data.frame(SquareFt = round(rbeta(70, 2, 5) * 8000))
df3 <- df3 %>% filter(SquareFt > 700)
set.seed(5 + 33)
df3$e <- rnorm(67, 1, 1)
df3$Price <- 100 * df3$SquareFt + df3$e * (df3$SquareFt) ^ 1.5

png(filename = "p12.png", height = 5, width = 6, units = "in", res = 600)
ggplot(df3, aes(x = SquareFt, y = Price)) + geom_point() + ylim(0, 1200000) + theme_bw()
dev.off()

lm3 <- lm(Price ~ SquareFt, data = df3)
summary(lm3)

png(filename = "p13.png", height = 5, width = 6, units = "in", res = 600)
ggplot(df3, aes(x = SquareFt, y = Price)) + geom_point() + ylim(0, 1200000) +
  geom_abline(intercept = coef(lm3)[1], slope = coef(lm3)[2], color = "darkblue", lwd = 1.25) + theme_bw()
dev.off()

df3$Residuals <- lm3$residuals
df3$Fitted <- lm3$fitted.values

png(filename = "p14.png", height = 5, width = 6, units = "in", res = 600)
ggplot(df3, aes(x = Fitted, y = Residuals)) + geom_point() + 
  geom_hline(yintercept = 0, color = "darkred", lwd = 1.25, lty = 2) + theme_bw()
dev.off()

# Variable transformation:
df3$PriceTrans <- log(df3$Price)
df3$PriceTrans2 <- sqrt(df3$Price)
lm3Trans <- lm(PriceTrans ~ SquareFt, data = df3)
df3$TransResiduals <- lm3Trans$residuals
df3$TransFitted <- lm3Trans$fitted.values

png(filename = "p12Trans.png", height = 5, width = 6, units = "in", res = 600)
ggplot(df3, aes(x = SquareFt, y = PriceTrans)) + geom_point() + 
  geom_abline(intercept = coef(lm3Trans)[1], slope = coef(lm3Trans)[2], color = "darkblue", lwd = 1.25)+
  theme_bw() + labs(x = "Square Feet", y = "Log(Price)")
dev.off()

png(filename = "p14Trans.png", height = 5, width = 6, units = "in", res = 600)
ggplot(df3, aes(x = TransFitted, y = TransResiduals)) + geom_point() + ylim(-.9, .9) +
  labs(x = "Predicted", y = "Residual") + 
  geom_hline(yintercept = 0, color = "darkred", lwd = 1.25, lty = 2) + theme_bw()
dev.off()

# Coefficient Interpretation
summary(lm3)
summary(lm3Trans)

lm3TransPreds <- exp(predict(lm3Trans, newdata = data.frame(SquareFt = c(1120, 1121))))
lm3TransPreds[2] / lm3TransPreds[1]
exp(0.00036310)

########### Non-linear correlation example ###########
set.seed(999)
df4 <- data.frame(x = seq(-2, 2, .01), y = (seq(-2, 2, .01) + rnorm(nrow(df4), mean = 0, sd = .25))**2)
df4$y <- df4$y 

png(filename = "noCorr.png", height = 4, width = 5, units = "in", res = 600)
ggplot(df4, aes(x, y)) + geom_point() + geom_abline(intercept = 1.42118, slope = -0.04726, lwd = 1.25, color = "darkblue") + 
  theme_bw()
dev.off()

lm4 <- lm(y ~ x, data = df4)
summary(lm4)
sqrt(0.001616)
cor.test(df4$x, df4$y)


########### Non-linear correlation example2 ###########
df4 <- data.frame(x = c(seq(0.01, 1, .01), seq(1.01, 3, .05)), y = log(c(seq(0.01, 1, .01), seq(1.01, 3, .05))))
set.seed(99)
df4$err <- rnorm(140, mean = 0, sd = .25) * df4$x
df4$y <- df4$y + df4$err

png(filename = "noCorr2.png", height = 4, width = 5, units = "in", res = 600)
ggplot(df4, aes(x, y)) + geom_point(size = .9) + 
  geom_abline(intercept = -1.61726, slope = 1.17516, lwd = 1.25, color = "darkblue") + 
  theme_bw()
dev.off()

lm4 <- lm(y ~ x, data = df4)
summary(lm4)
cor.test(df4$x, df4$y)
cor.test(df4$x, df4$y, method = "spearman")

########### Statistical vs. practical significance ###########
set.seed(123)
m5 <- as.data.frame(MASS::mvrnorm(n = 2000, mu = c(10, 15), Sigma = matrix(c(1, .05, .05, 1), nrow = 2, byrow = TRUE)))

png(filename = "noCorr3.png", height = 4, width = 5, units = "in", res = 600)
ggplot(m5, aes(x = V1, y = V2)) + geom_point(size = .75) + theme_bw() + 
  labs(x = "Gene 1 Normalized Expression", y = "Gene 2 Normalized Expression")
dev.off()

cor.test(m5$V1, m5$V2)

0.06548 * sqrt(1998)/sqrt(1-(0.06548)^2)
2*pt(2.933, df = 1998, lower.tail = FALSE)

0.06548 ** 2

lm5 <- lm(V2 ~ V1, data = m5)
lm5_0 <- update(lm5, . ~ . - V1)
summary(lm5_0)
png(filename = "noCorr4.png", height = 4, width = 5, units = "in", res = 600)
ggplot(m5, aes(x = V1, y = V2)) + geom_point(size = .75) + theme_bw() + 
  geom_abline(intercept = 14.36278, slope = 0.06463, lwd = 1.25, color = "darkblue") +
  labs(x = "Gene 1 Normalized Expression", y = "Gene 2 Normalized Expression")
dev.off()

########### Spearman example ###########
# 
df6 <- data.frame(Concentration = seq(1, 16, 1))
set.seed(34)
df6$Rate <- 1 / (1 + exp(-df6$Concentration + 7 + rnorm(16, 0, .75)))
df6 <- df6[1:14,]

png(filename = "noCorr5.png", height = 4, width = 5, units = "in", res = 600)
ggplot(df6, aes(x = Concentration, y = Rate)) + geom_point() + theme_bw()
dev.off()

lm6 <- lm(Rate ~ Concentration, df6)
summary(lm6)

cor.test(df6$Concentration, df6$Rate)
cor.test(df6$Concentration, df6$Rate, method = "spearman")

xtable::xtable(df6)

df6$xRanks <- 1:nrow(df6)
df6$yRanks <- rank(df6$Rate)

sum((df6$xRanks - mean(df6$xRanks))^2)
sum((df6$yRanks - mean(df6$yRanks))^2)
225.5 / 227.5

sum((df6$xRanks - mean(df6$xRanks)) * (df6$yRanks - mean(df6$yRanks)))

########### Transformation of x ###########
set.seed(58)
df7 <- data.frame(x = runif(20, -2, 2))
df7$x <- round(df7$x, 2)
set.seed(53)
df7$y <- -(rnorm(20, 0, .15) + df7$x)**2 + 10
df7$y <- round(df7$y, 2)
df7$xSquared <- df7$x**2
lm7 <- lm(y ~ xSquared, data = df7)
summary(lm7)

df7Big <- data.frame(x = seq(-3, 3, .001))
df7Big$y <- coef(lm7)[1] + coef(lm7)[2]*df7Big$x**2

png(filename = "nonLinear1.png", height = 4, width = 5, units = "in", res = 600)
ggplot(df7, aes(x = x, y = y)) + geom_point() + theme_bw() + xlim(-2.25, 2.25) + ylim(6, 10.25) +
  labs(x = "Deviation from optimal quantity (thousands)", y = "Profit")
dev.off()

png(filename = "nonLinear2.png", height = 4, width = 5, units = "in", res = 600)
ggplot(df7, aes(x = x, y = y)) + geom_point() + xlim(-2.25, 2.25) + ylim(6, 10.25) +
  geom_line(data = df7Big, mapping = aes(x = x, y = y), lwd = 1.25, color = "darkblue") + 
  labs(x = "Deviation from optimal quantity (thousands)", y = "Profit") + theme_bw()
dev.off()

df7b <- df7 %>% select(`Deviation from Optimal` = x, `Profit Margin` = y)
write.csv(df7b, file = "df7b.csv", row.names = FALSE)
