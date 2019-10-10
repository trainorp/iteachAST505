############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture13_Variances"))

df1 <- data.frame(x = seq(0, 25, .001))
df1$y3 <- dchisq(x = df1$x, df = 3)                  
df1$y6 <- dchisq(x = df1$x, df = 6)
df1$y10 <- dchisq(x = df1$x, df = 10)

df1 <- df1 %>% gather(key = "df", value = "Density", -x)
df1$df <- gsub("y", "", df1$df)
df1$df <- factor(df1$df, levels = c(3, 6, 10))

png(file = "chi.png", height = 4, width = 6, units = "in", res = 300)
ggplot(df1, aes(x = x, y = Density, group = df, color = df)) + geom_line(lwd = 1) + theme_bw() + xlab("Value")
dev.off()

########### New instrument example ###########
set.seed(3)
x <- rnorm(n = 7, mean = 147.0288, sd = .01)
sd(x) * 1000

(6 * var(x)) / 12.59
(6 * var(x)) / 1.635
EnvStats::varTest(x, conf.level = .90)

########### Water example ###########
set.seed(33)
x <- rnorm(n = 10, mean = 5, sd = .5)
x <- round(x, 3)
paste(x, collapse = ", ")

df2 <- data.frame(x = x, dev = round(x - mean(x),4), sqDev = round(round(x - mean(x),4)**2,4))
sum(df2$sqDev)
round(sum(df2$sqDev) / 9,4)
sVar <- round(var(x), 4)

(9 * sVar) / 19.02
(9 * sVar) / 2.700

EnvStats::varTest(x, conf.level = .95)

# Testing:
(9 * sVar) / .1
(9 * sVar) / 0.1292376

EnvStats::varTest(x, alternative = "greater", conf.level = .95)

########### Some F-distributions ############
df3 <- data.frame(x = seq(0, 7, .001))
df3$y <- df(df3$x, df1 = 5, df2 = 2)
df3$Distribution <- "df1 = 5, df2 = 2"
df4 <- data.frame(x = df3$x)
df4$y <- df(df4$x, df1 = 5, df2 = 20)
df4$Distribution <- "df1 = 5, df2 = 20"
df5 <- data.frame(x = df3$x)
df5$y <- df(df5$x, df1 = 22, df2 = 22)
df5$Distribution <- "df1 = 22, df2 = 22"
df3 <- rbind(df3, df4, df5)

png(filename = "F.png", height = 4, width = 6, units = "in", res = 300)
ggplot(df3, aes(x = x, y = y, color = Distribution)) + geom_line(lwd = 1) + theme_bw()
dev.off()

########### New water example ###########
set.seed(33)
x <- rnorm(n = 10, mean = 5, sd = .5)
x <- round(x, 3)
set.seed(333)
y <- rnorm(n = 10, mean = 5, sd = .25)
y <- round(y, 3)
paste(y, collapse = ", ")
round(var(y), 4)

round(0.24295 / 0.08883, 4)
round(var(x) / var(y), 4)
pf(round(0.2430 / 0.0888, 4), df = 9, df2 = 9, lower.tail = FALSE)

var.test(x = x, y = y, alternative = "greater", conf.level = .9)
var.test(x = x, y = y, alternative = "greater", conf.level = .95)
var.test(x = x, y = y, conf.level = .90)
var.test(x = x, y = y, conf.level = .80)


########### MI Var power analyses ###########
fPower <- function(alpha = .05, sigma1, sigma2, n1, n2){
  a <- sigma1 ^ 2 / sigma2 ^ 2 * qf(1 - alpha / 2, n1 - 1, n2 - 1)
  b <- sigma1 ^ 2 / sigma2 ^ 2 * qf(alpha / 2, n1 - 1, n2 - 1)
  
  return(1 - pf(a, n1 - 1, n2 - 1) + pf(b, n1 - 1, n2 - 1))
}

fPower(.05, 80.095, 64.945, 10, 11)
