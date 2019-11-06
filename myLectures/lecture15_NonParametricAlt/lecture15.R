############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture15_NonParametricAlt"))

############ Non parametric ANOVA ############
set.seed(6)
x1 <- round(rchisq(10, df = 5), 2) + 3
x1[x1 == 7.13] <- 9.12
set.seed(66)
x2 <- sample(x1 + round(rnorm(10, 1, 1), 2), size = 10)
set.seed(666)
x3 <- sample(x1 + round(rnorm(10, 3, 1), 2), size = 10)
set.seed(6666)
x4 <- sample(x1 + round(rnorm(10, -1, 1), 2), size = 10)

df1 <- data.frame(x = c(x1, x2, x3, x4), Tissue = factor(c(rep("Lung", 10), rep("Heart", 10), rep("Kidney", 10), rep("Brain", 10))))

wilcox.test(x4, x3)
kruskal.test(x ~ Tissue, data = df1)

qqP1 <- data.frame(qqnorm(x1))
png(filename = "QQPlot.png", height = 4.5, width = 6, units = "in", res = 300)
ggplot(qqP1, aes(x = x, y = y)) + geom_point() + stat_smooth(method = lm, se = FALSE) + theme_bw() + 
  labs(x = "Theoretical", y = "Sample") + ggtitle("Q-Q Plot")
dev.off()

png(filename = "GE.png", height = 4, width = 6, units = "in", res = 300)
set.seed(2222)
ggplot(df1, aes(y = x, x = Tissue, group = Tissue, fill = Tissue)) + geom_boxplot(width = .75, alpha = .5) + 
  geom_jitter(width = .1) + theme_bw() + labs(y = "Gene Expression")
dev.off()


# Add ranks:
write.csv(df1, file = "df1.csv")
df1$Rank <- rank(df1$x)

# Calculate the T's: 
df1 %>% group_by(Tissue) %>% summarize(sr = sum(Rank), srn = sr**2 / length(Rank)) %>% as.data.frame()
df1 %>% group_by(Tissue) %>% summarize(sr = sum(Rank), srn = sr**2 / length(Rank)) %>% 
  summarize(sum(srn)) %>% as.data.frame()

# Test statistic:
round(12/(40*41) * 17928.4 - 3*41, 3)

pchisq(8.1834, 3, lower.tail = FALSE)

diffs <- expand.grid(x = df1$x[df1$Tissue == "Brain"], y = df1$x[df1$Tissue == "Kidney"])
median(diffs$y - diffs$x)
