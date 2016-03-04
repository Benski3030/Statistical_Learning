library(ggplot2)
## From Agresti(2007) p.39
M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(gender = c("F", "M"),
                    party = c("Democrat","Independent", "Republican"))
(Xsq <- chisq.test(M))  # Prints test summary


randX2 <- rchisq(300000, df=Xsq$parameter)
ggplot(data.frame(x=randX2)) +
  geom_density(aes(x=x), fill="grey", color="grey") +
  geom_vline(xintercept=Xsq$statistic, color="red") +
  geom_vline(xintercept=mean(randX2) +2*sd(randX2), linetype=2) + 
  theme_classic() + 
  ggtitle("Results of X2 Test")
