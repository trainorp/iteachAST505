############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)
library(ggrepel)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture10_DiffInMeans"))

############ Equal variance ############
set.seed(3)
x <- rnorm(10, mean = 10, sd = 3)
set.seed(33)
x <- c(x, rnorm(10, mean = 13, sd = 3))
x <- round(x, 2)
df1 <- data.frame(x = x, strain = c(rep("WT", 10), rep("New Strain", 10)))
names(df1)[1] <- "Root Dry Mass"
df1
t.test(x ~ strain, data = df1, var.equal = TRUE, conf.level = .9)

df1 %>% group_by(strain) %>% summarize(mean = round(mean(`Root Dry Mass`),3), sd = round(sd(`Root Dry Mass`),3)) %>% 
                                         as.data.frame()

############ Equal variance ############
# sal <- carData::Salaries
# sal[sal$rank == "AsstProf",]
# lm1 <- lm(salary ~ factor(discipline) + yrs.since.phd + yrs.service + sex, data = sal)
# summary(lm1)
immer <- MASS::immer
imSum <- immer %>% group_by(Var) %>% summarize(round(mean(Y1),3), round(sd(Y1),3), n = length(Y1)) %>% as.data.frame()

t.test(x = immer$Y1[immer$Var == "T"], y = immer$Y1[immer$Var == "V"], alternative = "greater", var.equal = TRUE)

############ Unequal variance ############
oxPLDF <- read.csv("~/gdrive/Athro/oxPL6/wide_data_20150529.csv")
miSum <- oxPLDF %>% group_by(migroup) %>% 
  summarize(n = length(plateletsT0), mean = round(mean(plateletsT0),3), sd = round(sd(plateletsT0),3)) %>% 
  as.data.frame()

t.test(x = na.omit(oxPLDF$plateletsT0[oxPLDF$migroup == "Type 1"]),
       y = na.omit(oxPLDF$plateletsT0[oxPLDF$migroup == "Type 2"]))
