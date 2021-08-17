##setting working directory ##
rm(list())
setwd("C:/Users/anura/Documents")


## geometric brownian function ##
GeometricBrownian<-function()
{
  paths<-13
  count<-3000
  interval<-5/count
  mean<-0.06
  sigma<-0.3
  sample<-matrix(0,nrow=(count+1),ncol=paths)
  for(i in 1:paths)
  {
    sample[1,i]<-100
    for(j in 2:(count+1))
    {
      sample[j,i]<-sample[j-1,i]*exp(interval*(mean-((sigma)^2)/2)+((interval)^.5)*rnorm(1)*sigma) #Expression for Geometric Brownian Motion
    }
  }	
  cat("E[W(2)] = ",mean(sample[2001,]),"\n")
  cat("E[W(5)] = ",mean(sample[3001,]),"\n")
  matplot(sample,main="Geometric Brownian",xlab="Time",ylab="Path",type="l")
}
GeometricBrownian()



## another method for geometric brownian function
install.packages("tidyverse")
library(tidyverse)
nsim <- 50
t <- 100
mu <- 0
sigma <- 0.1
S0 <- 100
gbm <- gbm_loop(nsim, t, mu, sigma, S0)
gbm_df <- as.data.frame(gbm) %>%
  mutate(ix = 1:nrow(gbm)) %>%
  pivot_longer(-ix, names_to = 'sim', values_to = 'price')
gbm_df %>%
  ggplot(aes(x=ix, y=price, color=sim)) +
  geom_line() +
  theme(legend.position = 'none')
