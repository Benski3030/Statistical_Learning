#Fun with P Values

library(ggplot2)
library(fitdistrplus)
library(dplyr)
theme_set(theme_bw())

set.seed(132533)
our_population <- rnorm(10000, 15, 2.5)
qplot(our_population)

descdist(our_population, discrete = F, boot = 500)

psampler <- function (x, boot = 500, mean = 0, test_size = 100) {
  pvals <- c()
  bootstrap <- seq(0, boot)
  for (i in 1:length(bootstrap)) {
    rand_sample <- sample(x, size = test_size, replace = FALSE)
    test <- t.test(rand_sample, mu = mean)
    pval <- test$p.value
    pvals <- append(pvals, pval)
  }
  df <- data.frame(samp_num = bootstrap, p_val = pvals)
  psub <- subset(pvals, pvals > 0.05)
  print(paste("The bootstrapped false positive rate is:",round(length(psub)/length(bootstrap),5)*100,"%"))
  return(df)
}

pvals <- psampler(our_population, boot = 1000, mean = 14, test_size = 60)
qplot(pvals$p_val)
