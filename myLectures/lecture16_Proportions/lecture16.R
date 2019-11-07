############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture16_Proportions"))

########### Nausea ###########
piHat <- 8 / 24
piHat <- round(piHat, 4)

sePi <- sqrt((piHat * (1-piHat)) / 24)
round(sePi, 4)
round(qnorm(1-.10/2),3)

c(round(piHat - round(sePi, 4) * round(qnorm(1-.10/2),3), 4), 
  round(piHat + round(sePi, 4) * round(qnorm(1-.10/2),3), 4))

seNull <- sqrt(0.5*0.5 / 24)
(piHat - 0.50) / seNull
pnorm((piHat - 0.50) / seNull)

prop.test(8, 24, conf.level = .90, correct = FALSE)
binom.test(8, 24, alternative = "less")

round(round(qnorm(1-.10/2),3) ** 2,3)
8 + 0.5*(2.706) 
y2 <- 8 + 0.5*(2.706) 
24 + 2.706
n2 <- 24 + 2.706
(8 + 0.5*(2.706)) / (24 + 2.706)
1-.350

pi2 <- round(y2 / n2, 3)

round(sqrt((0.350 * 0.650) / 26.706), 4)
sePi2 <- round(sqrt((0.350 * 0.650) / 26.706), 4)

c(pi2 - 1.645 * sePi2, pi2 + 1.645 * sePi2)

# Adjusted when y = 0 
(3/8) / (4 + 3/4)
1 - (.05 / 2)^(1/4)

(0.3333-0.5000)/0.0962
prop.test(8, 24, p = .5, alternative = "less", correct = FALSE)

pnorm((0.3333-0.5000)/0.0962, lower.tail = TRUE)

pbinom(8, 24, .5)
pbinom(8, 24, .5, lower.tail = FALSE)
1 - pbinom(7, 24, .5, lower.tail = TRUE)


pbinom(3, 24, .5, lower.tail = FALSE)
1 - pbinom(2, 24, .5, lower.tail = TRUE)
1 - (dbinom(0, 24, .5) + dbinom(1, 24, .5) + dbinom(2, 24, .5))

sum(sapply(0:8, function(x) dbinom(x, 24, .5)))
pbinom(8, 24, .5, lower.tail = TRUE)

########### Washing machines ##########
ys <- c(278, 722)
ns <- c(1207,2409)
props <- round(ys / ns, 3)
inProps <- 1 - round(ys / ns, 3)

round(sqrt((props[1] * inProps[1]) / ns[1] + (props[2] * inProps[2]) / ns[2]), 4)

props[1] - props[2]
prop.test(c(278, 722), c(1207,2409), correct = FALSE)

round(c(props[1] - props[2] - 1.96 * 0.0153, props[1] - props[2] + 1.96 * 0.0153), 3)

# Test statistic;
(props[1] - props[2]) / 0.0153
round(2 * pnorm((props[1] - props[2]) / 0.0153), 5)

p1 <- ys / ns
q1 <- 1- p1

(p1[1] - p1[2]) / sqrt(p1[1]*q1[1] / ns[1] + p1[1]*q1[1] / ns[2])

prop.test(c(94, 113), c(125, 175), correct = FALSE)
sqrt(3.8509)

########### Fisher's exact test ###########
x1 <- matrix(c(20, 5, 27, 1), byrow = TRUE, ncol = 2)
fisher.test(x1, alternative = "less")

