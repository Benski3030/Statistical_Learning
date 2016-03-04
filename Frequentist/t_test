#The humble t test
library(tidyr)
library(dplyr)
library(ggplot2)

beer = c(27, 19, 20, 20, 23, 17, 21, 24, 31, 26, 28, 20, 27, 19, 25, 31, 24, 28, 24, 29, 21, 21, 18, 27, 20)
water = c(21, 19, 13, 22, 15, 22, 15, 22, 20, 12, 24, 24, 21, 19, 18, 16, 23, 20)
test <- data.frame(cbind(beer,water))

testx <- gather(test, "group", "count", 1:2)

ttest <- t.test(count ~ group, testx)
ttest

randT <- rt(30000, df=NROW(testx) - 1)

qplot(group, count, data=testx, geom="boxplot") +
  theme_classic() 

ggplot(data.frame(x=randT)) +
  geom_density(aes(x=x), fill="grey", color="grey") +
  geom_vline(xintercept=ttest$statistic, color="red") +
  geom_vline(xintercept=mean(randT) + c(-2, 2)*sd(randT), linetype=2) + 
  theme_classic()
