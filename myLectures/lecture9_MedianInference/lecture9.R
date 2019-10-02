############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)
library(ggrepel)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture9_MedianInference"))

############ Homework problem Glioblastoma ############
set.seed(12345)
x <- round(rexp(15, 1), 2) * 12
x[4] <- .25
boxplot(x)
wilcox.test(x, alternative = "two.sided", conf.int = TRUE, conf.level = .9)
wilcox.test(x, alternative = "less", mu = 12, conf.int = TRUE)

t.test(x, alternative = "two.sided", mu = 5, conf.int = TRUE, conf.level = .9)
t.test(x, alternative = "greater", mu = 5, conf.int = TRUE)

############ Homework problem t-test diff ############
set.seed(54321)
x <- round(rnorm(20, 55, 10))
set.seed(5432)
y <- round(rnorm(20, 70, 50))

t.test(x, y, conf.level = .9)
t.test(x, y)

############ Why the median ############
trueMed <- 40924

set.seed(33)
x <- round(rnorm(6, mean = trueMed, sd = 12000),0)
median(x)
mean(x)

x2 <- c(x, 419640)
median(x2)
mean(x2)

x2 <- x2[order(x2)]
df1 <- data.frame(x = sapply(1:7, function(i) qnorm((i - 0.5)/7)), y = x2)
png(filename = "qq1.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df1, aes(x = x, y = y)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) +
  theme_bw() + ggtitle("Q-Q Plot") + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Theoretical Normal quantiles", y = "Sample quantiles")
dev.off()

set.seed(333)
x3 <- rnorm(25)
x3 <- x3[order(x3)]
df2 <- data.frame(x = sapply(1:25, function(i) qnorm((i - 0.5)/25)), y = x3)
png(filename = "qq2.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df2, aes(x = x, y = y)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) +
  theme_bw() + ggtitle("Q-Q Plot") + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Theoretical Normal quantiles", y = "Sample quantiles")
dev.off()

############ Normal versus not normal ############
p1 <- ggplot(data.frame(x = c(-3.5, 3.5)), aes(x)) + 
  stat_function(fun = function(x) dnorm(x), lwd = 1, n = 50000, color = "darkblue") +
  theme_bw() + labs(x = "", y = "Density") + ggtitle("A Normal distribution") +
  theme(plot.title = element_text(hjust = 0.5)) 

p2 <- ggplot(data.frame(x = c(0, 2)), aes(x)) + 
  stat_function(fun = function(x) dgamma(x, 3, 6), lwd = 1, n = 50000, color = "darkred") +
  theme_bw() + labs(x = "", y = "Density") + ggtitle("Not a Normal distribution") +
  theme(plot.title = element_text(hjust = 0.5)) 

png(filename = "NormNotNorm.png", height = 5.5, width = 7, units = "in", res = 600)
show(gridExtra::grid.arrange(p1, p2))
dev.off()

############ Median confidence interval ############
phos <- robustbase::phosphor
phos$plant[16] <- 222
phos$plant[14] <- 383
qqPhos <- qqnorm(phos$plant)

qqPhosDF <- data.frame(x = qqPhos$x, y = qqPhos$y)

png(filename = "phosQQ.png", height = 4, width = 6, units = "in", res = 600)
ggplot(qqPhosDF, aes(x = x, y = y)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) +
  theme_bw() + ggtitle("Q-Q Plot") + theme(plot.title = element_text(hjust = 0.5)) + 
  labs(x = "Theoretical Normal quantiles", y = "Sample quantiles")
dev.off()

pp <- phos$plant[order(phos$plant)]

# Sign test:
w <- pp - 75
B <- sum(w > 0)
wilcox.test(pp, mu = 75, alternative = "greater", conf.int = TRUE, correct = FALSE)
pnorm((12-9) / sqrt(18/4), lower.tail = FALSE)

w2 <- pp - 100
B2 <- sum(w2 > 0)
wilcox.test(pp, mu = 100, alternative = "less", conf.int = TRUE)

############ Melanoma tumors ############
mel <- boot::melanoma
melDied <- mel[mel$status == "1",]
melDied$thickness

w <- melDied$thickness - 1.94
B <- sum(w > 0)

B2 <- (44 - 57/2) / sqrt(57/4)
