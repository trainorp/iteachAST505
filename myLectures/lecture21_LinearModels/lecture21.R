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

lmEthnicity <- lm(ldl ~ Ethnicity, data = df1)
summary(lmEthnicity)

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

png("cholSex2.png", height = 4, width = 6, units = "in", res = 300)
ggplot(df1Pred2, aes(x = totCholesterol, y = Predicted, color = Sex)) + geom_line(lwd = 1) +
  geom_point(data = df1, mapping = aes(x = totCholesterol, y = ldl, color = Sex), size = 1) + 
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

############ Diabetes data ############
diab <- read.csv("diabetes.csv")
diab$bmi <- 703 * diab$weight / diab$height^2

lm1 <- lm(glyhb ~ age + bmi + gender, data = diab)
summary(lm1)
plot(diab$age, diab$bmi)

############ carData::States ############
states <- carData::States
lm1 <- lm(SATM ~ pay * percent, data = states)
summary(lm1)

############ carData::MplsDemo ############
mpls <- carData::MplsDemo
lm1 <- lm(hhIncome ~ black * collegeGrad, data = mpls)
summary(lm1)

lm2 <- lm(hhIncome ~ foreignBorn + collegeGrad, data = mpls)
summary(lm2)

lm3 <- lm(hhIncome ~ foreignBorn * collegeGrad, data = mpls)
summary(lm3)

############ Reaction Rate versus temperature ############
x1 <- seq(300, 380, 5)
f1 <- function(a,b,c,x){
  return(a*x^2 + b*x + c)
}
plot(x1, f1(-0.0006875, 0.4675000, -78.3750000, x1))

y1 <- f1(-0.0006875, 0.4675000, -78.3750000, x1)
set.seed(99999)
y1 <- y1 + rnorm(length(y1), 0, .05)
y1 <- ifelse(y1 < 0, 0, y1)
plot(x1, y1)

df1 <- data.frame(x = x1, y = y1)
ggplot(df1, aes(x, y)) + geom_point() + theme_bw()
