############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)
library(ggrepel)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture14_ANOVA"))

############ ANOVA example ############
df1 <- expand.grid(Tree = c("Shortleaf Pine", "Sand Pine", "Eastern White Pine"), replication = 1:10, stringsAsFactors = FALSE)
df1$dbh <- NA
set.seed(1)
df1$dbh[df1$Tree == "Shortleaf Pine"] <- rnorm(10, 13.5, 2)
set.seed(2)
df1$dbh[df1$Tree == "Sand Pine"] <- rnorm(10, 9.1, 2)
set.seed(3)
df1$dbh[df1$Tree == "Eastern White Pine"] <- rnorm(10, 15.3, 2)

df2 <- expand.grid(Tree = c("Scarlet Oak", "Southern Red Oak", "White Oak"), replication = 1:10, stringsAsFactors = FALSE)
df2$dbh <- NA
set.seed(4)
df2$dbh[df2$Tree == "Scarlet Oak"] <- rnorm(10, 14.2, 2)
set.seed(5)
df2$dbh[df2$Tree == "Southern Red Oak"] <- rnorm(10, 14.1, 2)
set.seed(6)
df2$dbh[df2$Tree == "White Oak"] <- rnorm(10, 14.1, 2)

df1 <- rbind(df1, df2)
df1$Tree <- factor(df1$Tree)

lm1 <- lm(dbh ~ Tree, data = df1)

#png(filename = "Trees1.png", height = 4, width = 6, units = "in", res = 300)
p1 <- ggplot(df1 %>% filter(Tree %in% c("Shortleaf Pine", "Sand Pine", "Eastern White Pine")), 
       aes(y = dbh, color = Tree, x = Tree)) +
  stat_summary(geom = "point", fun.y = "mean", col = "black", fill = "black", size = 4, shape = 23) +
  geom_jitter(size = 1.5, width = .05) + theme_bw() + labs(y = "Diameter at Breast Height (Inches)") + ylim(6, 20) +
  scale_color_manual(values = RColorBrewer::brewer.pal(7, "Set1")[1:3])
show(p1)
#dev.off()

#png(filename = "Trees2.png", height = 4, width = 6, units = "in", res = 300)
p2 <- ggplot(df1 %>% filter(Tree %in% c("Scarlet Oak", "Southern Red Oak", "White Oak")), 
       aes(y = dbh, color = Tree, x = Tree)) +
  stat_summary(geom = "point", fun.y = "mean", col = "black", fill = "black", size = 4, shape = 23) +
  geom_jitter(size = 1.5, width = .05) + theme_bw() + labs(y = "Diameter at Breast Height (Inches)") + ylim(6, 20) +
  scale_color_manual(values = RColorBrewer::brewer.pal(7, "Set1")[c(4,5, 7)])
show(p2)
#dev.off()

df1 %>% filter(Tree %in% c("Shortleaf Pine", "Sand Pine", "Eastern White Pine")) %>% summarise(mean(dbh)) %>% as.data.frame()
df1 %>% filter(Tree %in% c("Scarlet Oak", "Southern Red Oak", "White Oak")) %>% summarise(mean(dbh)) %>% as.data.frame()

#png(filename = "Trees1b.png", height = 4, width = 6, units = "in", res = 300)
p1b <- p1 + geom_hline(yintercept = 12.81748, lwd = 1, lty = 2) + ggtitle("Greater Between-Group variability")
show(p1b)
#dev.off()

#png(filename = "Trees2b.png", height = 4, width = 6, units = "in", res = 300)
p2b <- p2 + geom_hline(yintercept = 14.52869, lwd = 1, lty = 2) + ggtitle("Lesser Between-Group variability")
show(p2b)
#dev.off()

#png(filename = "Trees3.png", height = 4*1.2, width = 10*1.2, units = "in", res = 300)
gridExtra::grid.arrange(p1b, p2b, nrow = 1) 
#dev.off()

df3 <- df1 
set.seed(888)
df3$dbh <- rnorm(60, 14.1, 8)
df3 %>% group_by(Tree) %>% summarize(vars = var(dbh)) %>% summarize(mean(vars))

df3 %>% group_by(Tree) %>% summarize(means = mean(dbh)) %>% summarize(10 * var(means)) 

anova1 <- aov(dbh ~ Tree, data = df3)
summary(anova1)

########### Back to the unequal case ###########
df1$dbh <- round(df1$dbh, 2)
df1b <- df1 %>% filter(Tree %in% c("Shortleaf Pine", "Sand Pine", "Eastern White Pine"))
df1c <- df1b %>% arrange(Tree)

df1b %>% group_by(Tree) %>% summarize(mean(dbh)) %>% as.data.frame()
df1b  %>% summarize(round(mean(dbh),3) )%>% as.data.frame()
10*(15.165 - 12.818)^2 + 10*(9.523-12.818)^2 + 10*(13.765-12.818)^2
172.622/2

df1b %>% group_by(Tree) %>% summarize(round(sd(dbh),3)) %>% as.data.frame()
9*(1.730)^2 + 9*(1.971)^2 + 9*(1.560)^2

83.802 / 27

86.311 / 3.103

paste(paste0(df1b$dbh, " - ", 12.818), collapse = ")^2 + (")

sum((df1b$dbh - 12.818)^2)

172.622 + 83.802

qf(.05, 2, 27, lower.tail = FALSE)

anova2 <- aov(dbh ~ Tree, data = df1b)
summary(anova2)

########### Post-hoc tests ###########
t.test(df1$dbh[df1$Tree =="Shortleaf Pine"], df1$dbh[df1$Tree == "Eastern White Pine"], var.equal = TRUE)
c(13.765-15.165 - qt(.975, 27) * sqrt(83.79 / 27 * (1/10 + 1/10)),
  13.765-15.165 + qt(.975, 27) * sqrt(83.79 / 27 * (1/10 + 1/10)))
DescTools::PostHocTest(anova2, method = "lsd")
DescTools::PostHocTest(anova2, method = "bonf")

pairwise.t.test(df1b$dbh, df1b$Tree, data = df1b, p.adjust.method = "none")
pairwise.t.test(df1b$dbh, df1b$Tree, data = df1b, p.adjust.method = "bonf")

########### HPV data  ###########
df1 <- data.frame(group = c("Group 1", "Group 2", "Group 3", "Malignant"), mean = c(-0.321, -0.863, -3.05, -2.907))
df1 <- df1 %>% slice(rep(1:n(), each = 10))
