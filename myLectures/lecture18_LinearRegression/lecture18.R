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
