############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)
library(ggrepel)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture11_Nonparametric"))

df1 <- data.frame(x = seq(0, 10, by = .001), `Cancer Stage` = "Stage 4")
df1$y <- dweibull(df1$x, shape = 1.5, scale = 3)
df2 <- data.frame(x = seq(0, 10, by = .001), `Cancer Stage` = "Stage 3")
df2$y <- dweibull(df2$x, shape = 1.5, scale = 3)
df2$x <- df2$x + 1.5
df1 <- rbind(df1, df2)
df1$x <- df1$x * 12

png(file = "locationShift.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df1, aes(x = x, y = y, color = Cancer.Stage)) + geom_line(lwd = 1.25) + xlim(0, 120) + theme_bw() +
  ggtitle("Time until death") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time (months)", y = "Density", color = "Cancer Stage") 
dev.off()

png(file = "locationShift2.png", height = 4, width = 6, units = "in", res = 600)
ggplot(df1, aes(x = x, y = y, color = Cancer.Stage)) + geom_line(lwd = 1.25) + 
  geom_segment(aes(x = 17, xend = 35, y = .26, yend = .26), 
               arrow = arrow(length = unit(0.20,"cm"), ends="both", type = "closed"), color = "black") +
  annotate("label", x = 26, y = .275, label = expression(Delta)) +
  xlim(0, 120) + ylim(0, .285) + theme_bw() +
  ggtitle("Time until death") + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Time (months)", y = "Density", color = "Cancer Stage") 
dev.off()

########### Simulated Survival times ########### 
set.seed(3)
x1 <- round(rweibull(6, shape = 1.5, scale = 3) * 12,1)
set.seed(333)
x2 <- round((rweibull(6, shape = 1.5, scale = 3) + 2.5) * 12,1)
df3 <- data.frame(time = c(x1, x2), stage = c(rep("Stage 4", times = 6), rep("Stage 3", times = 6)))

# Boxplot:
png(file = "boxplot.png", height = 4, width = 6, units = "in", res = 600)
set.seed(333)
ggplot(df3, aes(x = stage, y = time, fill = stage)) + geom_boxplot() + geom_jitter(width = .05) + theme_bw()
dev.off()

# Tests:
wilcox.test(x = x1, y = x2)
test1 <- wilcox.test(time ~ stage, data = df3, exact = TRUE, alternative = "greater", conf.int = TRUE)
test1$statistic + 6 * (6 + 1) / 2
test1

# Point estimate:
combList <- expand.grid(x1 = 1:6, x2 = 1:6)
combList$diff <- combList$x2Val <- combList$x1Val <- NA
for(i in 1:nrow(combList)){
  combList$x1Val[i] <- x1[combList$x1[i]]
  combList$x2Val[i] <- x2[combList$x2[i]]
}
combList$diff <- combList$x1Val - combList$x2Val

# Confidence intervals:
sort(combList$diff)

wilcox.test(time ~ stage, data = df3, exact = TRUE, alternative = "two.sided", conf.int = TRUE)


########### Simulated data for normal appoximation ############
# micrograms per liter as phycocyanin 
set.seed(2)
x1 <- round(exp(rnorm(n = 100, mean = log(222), sd = 1/2)), 1)
set.seed(22)
x2 <- round(exp(rnorm(n = 100, mean = log(300), sd = 1/2)), 1)
df4 <- data.frame(value = c(x1, x2), month = c(rep("June", 100), rep("August", 100)))
df4$month <- factor(df4$month, levels = c("June", "August"))

png(file = "RoughHist.png", height = 5, width = 6.5, units = "in", res = 600)
ggplot(df4, aes(x = value, y = ..density..)) + geom_histogram(color = "black", fill = "grey70") + 
  facet_wrap(~month, nrow = 2) + theme_bw() + labs(x = expression(paste(mu, "g per liter")), y = "Density")
dev.off()

df4 <- df4 %>% arrange(value)
df4Counts <- df4 %>% group_by(value) %>% summarize(n = n())
df4 <- df4 %>% left_join(df4Counts)
df4$ranks <- rank(df4$value, ties = "average")
sum(df4$ranks[df4$month == "June"])
