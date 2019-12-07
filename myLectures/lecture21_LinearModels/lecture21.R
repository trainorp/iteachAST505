############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)
library(rgl)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture21_LinearModels"))

############ Examples ############
set.seed(333)
df1 <- data.frame(x = runif(50, -3, 3))

set.seed(3332)
df1$y <- 3 + df1$x + rnorm(50, 0, .6)

set.seed(32)
df1$y2 <- 1 - 1.5*df1$x + 2.25 * df1$x^2 + rnorm(50, 0, 3)

set.seed(32)
df1$y3 <- (df1$x + .4)^3 + rnorm(50, 0, 4)

set.seed(3)
df1$x2 <- runif(50, -3, 3)

set.seed(32)
df1$y4 <- df1$x + df1$x2 + rnorm(50, 0, .5)

png(file = "lm1.png", height = 4, width = 5, units = "in", res = 300)
ggplot(df1, aes(x = x, y = y)) + stat_smooth(method = "lm") + geom_point() + 
  geom_vline(xintercept = 0, lty = 2) + theme_bw()
dev.off()
lm1 <- lm(y ~ x, data = df1)
summary(lm1)

png(file = "lm2.png", height = 4, width = 5, units = "in", res = 300)
ggplot(df1, aes(x = x, y = y2)) + stat_smooth(method = "lm", formula = y ~ x + I(x^2)) + 
  geom_point() + geom_vline(xintercept = 0, lty = 2) + theme_bw() + labs(y = "y")
dev.off()
lm2 <- lm(y2 ~ x + I(x^2), data = df1)
summary(lm2)

png(file = "lm3.png", height = 4, width = 5, units = "in", res = 300)
ggplot(df1, aes(x = x, y = y3)) + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3)) + 
  geom_point() + geom_vline(xintercept = 0, lty = 2) + theme_bw() + labs(y = "y")
dev.off()
lm3 <- lm(y3 ~ x + I(x^2) + I(x^3), data = df1)
summary(lm3)

set.seed(333)
df2 <- data.frame(x = runif(200, -3, 3))
set.seed(3)
df2$x2 <- runif(200, -3, 3)
set.seed(32)
df2$y <- .25 * df2$x + .25 * df2$x2 + rnorm(200, 0, .25)
plot3d(x = df2$x, y = df2$x2, z = df2$y, type = "s", r = .1)
car::scatter3d(x = df2$x, y = df2$x2, z = df2$y, surface = FALSE, xlab = "x1", ylab = "x2", zlab = "y")
car::scatter3d(x = df2$x, z = df2$x2, y = df2$y, xlab = "x1", ylab = "y", zlab = "x2", axis.scales = FALSE)
# rgl.snapshot(filename = "lm4_2.png")
lm4 <- lm(y ~ x + x2, data = df2)
summary(lm4)

############ Soils data ############
soil <- carData::Soils
soilMeans <- apply(soil[, 6:14], 2, mean)
soilCov <- cov(soil[, 6:14])
cor(carData::Soils[, 6:14])

set.seed(666-1)
soilSim <- round(MASS::mvrnorm(n = 14, mu = soilMeans, Sigma = soilCov), 3)
soilSim[2, "Conduc"] <- abs(soilSim[2, "Conduc"])
soilSim <- as.data.frame(soilSim)
write.csv(soilSim, "soilSim.csv", row.names = FALSE)

soil <- soilSim

lm1 <- lm(Conduc ~ Na + K, data = soil)
summary(lm1)

plot(lm1)

qqnorm(resid(lm1))
qqline(resid(lm1))

soilPred <- expand.grid(K = c(.2, .5, .8), Na = seq(2, 6, .001))
soilPred$Predicted <- predict(lm1, newdata = soilPred)

png("partialPlot.png", width = 5, height = 4, units = "in", res = 300)
ggplot(soilPred, aes(x = Na, y = Predicted, group = K, color = as.factor(K))) + geom_line(lwd = 1) + 
  labs(y = expression(paste("Predicted Conductivity     ", hat(y)[i])), color = "K") + theme_bw()
dev.off()

lm2 <- lm(Conduc ~ Na * Mg, data = soil)
summary(lm2)

############ Household income and home size ############
set.seed(3)
m1 <- MASS::mvrnorm(n = 250, mu = c(0, 0), Sigma = matrix(c(1.5, -.15, -.05, .1), byrow = TRUE, nrow = 2))
df1 <- data.frame(inc = 40000 * exp(m1[, 1]), sqFt = 2000 * exp(m1[, 2]))

df1 %>% summarize(mean(inc), median(inc), mean(sqFt), median(sqFt))
cor(as.matrix(df1), method = "spearman")

ggplot(df1, aes(x = inc, y = sqFt)) + geom_point() + theme_bw()

df1$inc <- df1$inc / 1000
df1$sqFt <- df1$sqFt / 1000
df1$price <- 40 + df1$inc + 1.4 * df1$sqFt + 1.5 * df1$inc*df1$sqFt
set.seed(33+333)
df1$err <- sapply(df1$price, function(x) rnorm(1, 0, x / 10))
df1$price <- df1$price + df1$err

df1$SquareFeet <- df1$sqFt
df1$HouseholdIncome <- df1$inc
lm1 <- lm(price ~ SquareFeet * HouseholdIncome, data = df1)
summary(lm1)

df1Pred <- expand.grid(HouseholdIncome = c(40, 80, 120, 240), SquareFeet = seq(1, 3, .001))
df1Pred$Predicted <- predict(lm1, newdata = df1Pred)

png("houseInteract.png", height = 4, width = 6, units = "in", res = 300)
ggplot(df1Pred, aes(x = SquareFeet, y = Predicted, group = HouseholdIncome, color = as.factor(HouseholdIncome))) + 
  geom_line(lwd = 1) + theme_bw() + ylim(0, 1400) + 
  labs(x = "Size (in thousands of Square feet)",
  y = "Predicted Price (in thousands $)", color = "Household Income\n(in thousands $)")
dev.off()

ggplot(df1, aes(x = SquareFeet, y = price, color = HouseholdIncome)) + geom_point()

write.csv(df1, file = "Housing.csv")

############ Simulated LDL data ############
set.seed(69)
df1 <- data.frame(Age = round(rnorm(400, 60, 15),1), totCholesterol = rnorm(400, 180, 20))
df1$Ethnicity <- factor(c(rep("White/Caucasian", 100), rep("Black/African-American", 100),
                     rep("Asian-American", 100), rep("Hispanic/Latino", 100)))
df1$Sex <- factor(rep(c("Male", "Female"), each = 25))

set.seed(6969)
df1$error <- rnorm(400, 0, 10)
df1$ethAdd <- -80 + ifelse(df1$Ethnicity == "White/Caucasian", 0, 
                     ifelse(df1$Ethnicity == "Black/African-American", 6,
                            ifelse(df1$Ethnicity == "Asian-American", -2.5, 1.5)))
df1$sexAdd <- NA
df1$sexAdd[df1$Sex == "Male"] <- .15 * df1$totCholesterol[df1$Sex == "Male"]
df1$sexAdd[df1$Sex == "Female"] <- -.2 * df1$totCholesterol[df1$Sex == "Female"] + 30

df1$ldl <- -0.08 * df1$Age + 0.85 * df1$totCholesterol + df1$ethAdd + df1$sexAdd + df1$error
set.seed(33)
write.csv(df1[sample(1:nrow(df1), size = nrow(df1)),], file = "Chol.csv")

lmEthnicity <- lm(ldl ~ Ethnicity, data = df1)
summary(lmEthnicity)

anova(lmEthnicity)

df1Sum <- df1 %>% group_by(Ethnicity) %>% summarize(y = mean(ldl)) %>% as.data.frame()
df1Sum$x <- 1:4 - .15
df1Sum$xend <- 1:4 + .15

png("EthnScatter.png", height = 4, width = 5.5, units = "in", res = 300)
set.seed(999)
ggplot(df1, aes(x = Ethnicity, y = ldl)) + geom_jitter(width = .1, size = 1, color = "grey30") + 
  geom_segment(data = df1Sum, mapping = aes(x = x, xend = xend, y = y, yend = y), color = "red", lwd = 1.5) + 
  theme_bw() + labs(y = "LDL")
dev.off()

lm2 <- lm(ldl ~ Sex + totCholesterol, data = df1)
summary(lm2)
df1Pred1 <- expand.grid(Sex = c("Male", "Female"), totCholesterol = seq(110, 240, .01))
df1Pred1$Predicted <- predict(lm2, newdata = df1Pred1)

png("cholSex1.png", height = 4, width = 6, units = "in", res = 300)
ggplot(df1Pred1, aes(x = totCholesterol, y = Predicted, color = Sex)) + geom_line(lwd = 1) +
  geom_point(data = df1, mapping = aes(x = totCholesterol, y = ldl, color = Sex), size = 1) + 
  theme_bw() + labs(x = "Total Cholesterol", y = "LDL")
dev.off()

lm3 <- update(lm2, . ~ . + Sex:totCholesterol)
df1Pred2 <- expand.grid(Sex = c("Male", "Female"), totCholesterol = seq(110, 240, .01))
df1Pred2$Predicted <- predict(lm3, newdata = df1Pred2)

summary(lm3)
anova(lm3)
131679 + 96098 + 5791 + 41568
(233568 / 3)/(41568 / 396)
qf(.01, 3, 396, lower.tail = FALSE)
pf((233568 / 3)/(41568 / 396), 3, 396, lower.tail = FALSE)
qt(.025, 396, lower.tail = FALSE)
0.60479 - 1.9660 * (0.03783)
0.60479 + 1.9660 * (0.03783)

0.80466 - 1.9660 * (0.02835)
0.80466 + 1.9660 * (0.02835)
confint(lm2)

png("cholSex2.png", height = 4, width = 6, units = "in", res = 300)
ggplot(df1Pred2, aes(x = totCholesterol, y = Predicted, color = Sex)) + geom_line(lwd = 1) +
  geom_point(data = df1, mapping = aes(x = totCholesterol, y = ldl, color = Sex), size = 1) + 
  theme_bw() + labs(x = "Total Cholesterol", y = "LDL") 
dev.off()

png("cholSex2b.png", height = 4, width = 6, units = "in", res = 300)
ggplot(df1Pred2, aes(x = totCholesterol, y = Predicted, color = Sex)) + geom_line(lwd = 1) +
  geom_point(data = df1, mapping = aes(x = totCholesterol, y = ldl, color = Sex), size = 1) +
  geom_vline(xintercept = 0, lwd = .5, lty = 2) +
  geom_hline(yintercept = 0, lwd = .5, lty = 2) +
  theme_bw() + labs(x = "Total Cholesterol", y = "LDL") + xlim(0, 300)
dev.off()

segmentDF <- data.frame(x = c(135.00, 200.00), y = c(37.04158, 76.35270), 
                        xend = c(135.00, 200.00), yend = c(51.96548, 116.95804))
png("cholSex2c.png", height = 4, width = 6, units = "in", res = 300)
ggplot(df1Pred2, aes(x = totCholesterol, y = Predicted, color = Sex)) + geom_line(lwd = 1) +
  geom_point(data = df1, mapping = aes(x = totCholesterol, y = ldl, color = Sex), size = 1) + 
  geom_segment(data = segmentDF, mapping = aes(x = x, y = y, xend = xend, yend = yend,
               color = NULL), lwd = 1, arrow = arrow(length = unit(0.1, "inches"))) +
  annotate(geom = "label", x = 147, y = 51, label= "39.31") +
  annotate(geom = "label", x = 211, y = 100, label= "64.99") +
  theme_bw() + labs(x = "Total Cholesterol", y = "LDL") 
dev.off()

lm4 <- lm(ldl ~ Age + Sex * totCholesterol + Ethnicity, data = df1)
summary(lm4)
lm0 <- lm(ldl ~ 1, data = df1)
anova(lm0, lm4)

df1Pred3 <- expand.grid(Age = mean(df1$Age), Ethnicity = "Asian-American", Sex = c("Male", "Female"),
                      totCholesterol = seq(100, 240, .01))
df1Pred3$Predicted <- predict(lm4, newdata = df1Pred3)

ggplot(df1Pred3, aes(x = totCholesterol, y = Predicted, color = Sex)) + geom_line() + theme_bw()

########### Collinearity ###########
set.seed(999)
m1 <- MASS::mvrnorm(n = 100, mu = c(75, 75, 75, 75), 
      Sigma = matrix(100 * c(1, .95, -.20, -.20,
                             .95, 1, -.20, -.20,
                             -.20, -.20, 1, .95, 
                             -.20, -.20, .95, 1), 
                     byrow = TRUE, nrow = 4))
m1 <- round(m1)
PsychData <- as.data.frame(m1)
cor(PsychData)
names(PsychData) <- c("Angry", "ShortTempered", "Compassionate", "Kind")

write.csv(PsychData, file = "Psych.csv")

lmAngry <- lm(Angry ~ Compassionate, data = PsychData)
summary(lmAngry)

lmAngry1 <- lm(Angry ~ Kind, data = PsychData)
summary(lmAngry1)

lmAngry2 <- lm(Angry ~ Compassionate + Kind, data = PsychData)
summary(lmAngry2)

png(filename = "corplot1.png", height = 5, width = 6, units = "in", res = 300)
corrplot::corrplot(cor(PsychData), diag = FALSE, addCoef.col = "black", method = "ellipse",
                   tl.col = "black", col = cm.colors(100))
dev.off()

lmAngry3 <- lm(Angry ~ Compassionate + ShortTempered, data = PsychData)
summary(lmAngry3)
