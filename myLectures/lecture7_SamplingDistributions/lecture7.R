############ Prereqs ############
options(stringsAsFactors = FALSE, scipen = 600)
oldPar <- par()
os <- Sys.info()["sysname"]
baseDir <- ifelse(os == "Windows", "C:/Users/ptrainor/gdrive", "~/gdrive")

library(tidyverse)
library(ggrepel)

setwd(paste0(baseDir, "/iTeach/AST_505/myLectures/lecture7_SamplingDistributions"))

########### Sampling distributions ############
set.seed(33)
fishWeights <- rnorm(100, 500, 10)
fishweights <- round(fishWeights, 2)
mean(fishweights)

varp <- function(x) mean((x-mean(x))^2)
sqrt(varp(fishweights))

set.seed(333)
fishSamps <- replicate(5, sample(fishweights, 10))
SampleMeans <- apply(fishSamps, 2, mean)

colnames(fishSamps) <- paste0("Sample ", 1:5)
rownames(fishSamps) <- paste0("Fish ", 1:10)
rbind(fishSamps, SampleMeans)

set.seed(333)
fishSamps <- replicate(10000, sample(fishweights, 10))
SampleMeans <- apply(fishSamps, 2, mean)
fishDF <- data.frame(SampleMeans = SampleMeans)
png(file = "sampleMeanHist.png", height = 4, width = 6, units = "in", res = 600)
ggplot(fishDF, aes(x = SampleMeans)) + geom_histogram(bins = 40, color = "black", fill = "grey65") + 
  theme_bw() + labs(x = "Sample Mean", y = "Frequency") + ggtitle("Sample means of 1,000 random samples") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


set.seed(333)
fishSamps <- replicate(10000, sample(fishweights, 5))
SampleMeans <- apply(fishSamps, 2, mean)
set.seed(333)
fishSamps <- replicate(10000, sample(fishweights, 10))
SampleMeans <- c(SampleMeans, apply(fishSamps, 2, mean))
set.seed(333)
fishSamps <- replicate(10000, sample(fishweights, 50))
SampleMeans <- c(SampleMeans, apply(fishSamps, 2, mean))
fishDF2 <- data.frame(SampleMeans = SampleMeans, SampleSize = rep(c("5", "10", "50"), each = 10000))
fishDF2$SampleSize <- factor(fishDF2$SampleSize, levels = c("5", "10", "50"))

png(file = "sampleMeanHist2.png", height = 5, width = 6, units = "in", res = 600)
ggplot(fishDF2, aes(x = SampleMeans, y = ..density.., fill = SampleSize)) + geom_histogram(bins = 40, color = "black") + 
  facet_wrap(~SampleSize, nrow = 3, scales = "free_y") +
  theme_bw() + labs(x = "Sample Mean", y = "Density", fill = "Sample Size\n(n)") + ggtitle("Sample means of 1,000 random samples") +
  theme(plot.title = element_text(hjust = 0.5)) 
dev.off()


fishDF2 %>% group_by(SampleSize) %>% summarize(Mean = mean(SampleMeans), SD = sd(SampleMeans)) %>% as.data.frame()

########### Gamma distribution example ############
set.seed(3333)
gamDF <- data.frame(x = rgamma(10000, shape = 1, scale = 10))
gamDF$SampleSize <- "Population"
set.seed(33333)
gamDF <- rbind(gamDF, data.frame(x = apply(replicate(10000, sample(gamDF$x, 3)), 2, mean), SampleSize = "n = 3"))
set.seed(33333)
gamDF <- rbind(gamDF, data.frame(x = apply(replicate(10000, sample(gamDF$x, 10)), 2, mean), SampleSize = "n = 10"))
set.seed(33333)
gamDF <- rbind(gamDF, data.frame(x = apply(replicate(10000, sample(gamDF$x, 50)), 2, mean), SampleSize = "n = 50"))
set.seed(33333)
gamDF <- rbind(gamDF, data.frame(x = apply(replicate(10000, sample(gamDF$x, 100)), 2, mean), SampleSize = "n = 100"))
set.seed(33333)
gamDF <- rbind(gamDF, data.frame(x = apply(replicate(10000, sample(gamDF$x, 250)), 2, mean), SampleSize = "n = 250"))
gamDF$SampleSize <- factor(gamDF$SampleSize, levels = c("Population", "n = 3", "n = 10", "n = 50", "n = 100", "n = 250"))

png(file = "smHist1.png", height = 4, width = 6, units = "in", res = 600)
ggplot(gamDF %>% filter(SampleSize == "Population"), aes(x = x, group = SampleSize, color = SampleSize, fill = SampleSize)) + 
  geom_density(adjust = 1.75, lwd = 1.25, alpha = .1) + theme_bw() + xlim(-1, 30) + 
  labs(x = "Plasminogen concentration (mg/dL)", color = "Distribution", fill = "Distribution") + 
  ggtitle("Population or sample mean of plasminogen concentration") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")
dev.off()

png(file = "smHist2.png", height = 4, width = 6, units = "in", res = 600)
ggplot(gamDF %>% filter(SampleSize %in% c("Population", "n = 3")), aes(x = x, group = SampleSize, 
                                                                       color = SampleSize, fill = SampleSize)) + 
  geom_density(adjust = 1.75, lwd = 1.25, alpha = .1) + theme_bw() + xlim(0, 30) + 
  labs(x = "Plasminogen concentration (mg/dL)", color = "Distribution", fill = "Distribution") +
  ggtitle("Population or sample mean of plasminogen concentration") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")
dev.off()

png(file = "smHist3.png", height = 4, width = 6, units = "in", res = 600)
ggplot(gamDF %>% filter(SampleSize %in% c("Population", "n = 3", "n = 10")), aes(x = x, group = SampleSize, 
                                                                                 color = SampleSize, fill = SampleSize)) + 
  geom_density(adjust = 1.75, lwd = 1.25, alpha = .1) + theme_bw() + xlim(0, 30) + 
  labs(x = "Plasminogen concentration (mg/dL)", color = "Distribution", fill = "Distribution") +
  ggtitle("Population or sample mean of plasminogen concentration") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")
dev.off()

png(file = "smHist4.png", height = 4, width = 6, units = "in", res = 600)
ggplot(gamDF %>% filter(SampleSize %in% c("Population", "n = 3", "n = 10", "n = 50")), aes(x = x, group = SampleSize, 
                                                                                           color = SampleSize, fill = SampleSize)) + 
  geom_density(adjust = 1.75, lwd = 1.25, alpha = .1) + theme_bw() + xlim(0, 30) + 
  labs(x = "Plasminogen concentration (mg/dL)", color = "Distribution", fill = "Distribution") +
  ggtitle("Population or sample mean of plasminogen concentration") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")
dev.off()

png(file = "smHist5.png", height = 4, width = 6, units = "in", res = 600)
ggplot(gamDF %>% filter(SampleSize %in% c("Population", "n = 3", "n = 10", "n = 50", "n = 100")), aes(x = x, group = SampleSize, 
                                                                                                      color = SampleSize, fill = SampleSize)) + 
  geom_density(adjust = 1.75, lwd = 1.25, alpha = .1) + theme_bw() + xlim(0, 30) + 
  labs(x = "Plasminogen concentration (mg/dL)", color = "Distribution", fill = "Distribution") +
  ggtitle("Population or sample mean of plasminogen concentration") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") 
dev.off()

png(file = "smHist6.png", height = 4, width = 6, units = "in", res = 600)
ggplot(gamDF, aes(x = x, group = SampleSize, color = SampleSize, fill = SampleSize)) + 
  geom_density(adjust = 1.75, lwd = 1.25, alpha = .1) + theme_bw() + xlim(0, 30) + 
  labs(x = "Plasminogen concentration (mg/dL)", color = "Distribution", fill = "Distribution") +
  ggtitle("Population or sample mean of plasminogen concentration") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")
dev.off()

