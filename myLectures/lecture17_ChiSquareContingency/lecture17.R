############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture17_ChiSquareContingency"))

# First poll:
37.5+34.5-100
dmultinom(c(4, 4, 2), 10, prob = c(.375, .345, .280))
factorial(10) / (factorial(4) * factorial(4) * factorial(2)) * .375^4 * .345^4 * .280^2
200 * c(.375, .345, .280)

# Second poll:
57.3 + 33.2 - 100
55 + 33 - 100
42 + 9 - 100

# Germantown:
(55-57.3)^2 / 57.3
(33-33.2)^2 / 33.2
(12-9.5)^2 / 9.5

(55-57.3)^2 / 57.3 + (33-33.2)^2 / 33.2 + (12-9.2)^2 / 9.2
0.09232 + 0.00120 + 0.65789
pchisq(0.09232 + 0.00120 + 0.65789, df = 2, lower.tail = FALSE)
chisq.test(x = c(55, 33, 12), p = c(.573, .332, .095), correct = FALSE)
chisq.test(x = c(55, 33, 12), p = c(.573, .332, .095), simulate.p.value = TRUE, B = 10000)

# South End:
(42-57.3)^2 / 57.3
(49-33.2)^2 / 33.2
(9-9.5)^2 / 9.5
4.0853 + 7.5193 + 0.02632
pchisq(11.63092, df = 2, lower.tail = FALSE)
chisq.test(x = c(42, 49, 9), p = c(.573, .332, .095), correct = FALSE)

########## Chi square independence ########## 
# Example of independence:
colProps <- c(.2, .3, .35, .15)
rowProps <- c(.20, .35, .45)
t(t(colProps)) %*% t(rowProps)

nTable <- 2000 * t(t(colProps)) %*% t(rowProps)
rowSums(nTable)
colSums(nTable)

#### Hospital data:
hosp <- vcd::Hospital
round(prop.table(hosp), 3)
round(prop.table(hosp, margin = 1), 3)
round(prop.table(hosp, margin = 2), 3)

# Margins:
rowSums(hosp)
colSums(hosp)
sum(colSums(hosp))
round(prop.table(hosp), 3)
round(rowSums(hosp) / sum(rowSums(hosp)), 3)
round(colSums(hosp) / sum(colSums(hosp)), 3)

# RR:
qchisq(.05, df = 4, lower.tail = FALSE)

# Estimated expected value:
expProp <- round(t(t(round(rowSums(hosp) / sum(rowSums(hosp)), 4))) %*% t(round(colSums(hosp) / sum(colSums(hosp)), 4)), 4)
expN <- 132*expProp
colSums(expN)
rowSums(expN)
sum(rowSums(expN))
xs <- round((hosp - expN)^2 / expN, 4)
colSums(xs)
rowSums(xs)
sum(xs)
pchisq(sum(xs), df = 4, lower.tail = FALSE)

chisq.test(hosp, correct = FALSE)

#### ApoCI genotype:
m1 <- matrix(c(14, 99, 144, 5, 87, 150), nrow = 3)
t1 <- as.table(m1)
dimnames(t1)[[1]] <- c("AA", "AB", "BB")
dimnames(t1)[[2]] <- c("AD", "No AD")
t1
rowSums(t1)
colSums(t1)

round((t(t(rowSums(t1))) %*% t(colSums(t1))) / 499, 4)
rowSums(round((t(t(rowSums(t1))) %*% t(colSums(t1))) / 499, 4))
colSums(round((t(t(rowSums(t1))) %*% t(colSums(t1))) / 499, 4))

chiParts <- (m1 - round((t(t(rowSums(t1))) %*% t(colSums(t1))) / 499, 4)) ^ 2 / round((t(t(rowSums(t1))) %*% t(colSums(t1))) / 499, 4)
sum(chiParts)
pchisq(4.7131, 2, lower.tail = FALSE)
chisq.test(t1, simulate.p.value = FALSE)
chisq.test(t1, simulate.p.value = TRUE, B = 10000)
