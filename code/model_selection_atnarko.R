library(here);library(ggplot2);library(rstan)

source(here('code','stan_functions.R'))
source(here('code','lfo_stan_functions.R'))
source(here('code','util_functions.R'))
source(here('code','RP_functions.R'))

SR_data <- read.csv(here('data','Atnarko_SK_RS.csv'))
names(SR_data)[1]=c('broodyear')
SR_data$logR_S<- log(SR_data$recruits/SR_data$spawners)
SR_data<- SR_data[complete.cases(SR_data$logR_S),]

ggplot(SR_data, aes(spawners, recruits)) +
  geom_point(aes(colour = broodyear),size=2.5) +
  scale_colour_viridis_c()

ll1<- stan_lfo_cv(mod=m1,type='static',df=SR_data,L=15)
#model 2 - static autocorrelated Ricker
ll2<- stan_lfo_cv(mod=m2,type='tv',df=SR_data,L=15)
#model 3 - dynamic productivity Ricker
ll3<- stan_lfo_cv(mod=m3,type='tv',df=SR_data,L=15)

#model 4 - dynamic capacity Ricker
ll4<- stan_lfo_cv(mod=m4,type='tv',df=SR_data,L=15)

#model 5 - dynamic productivity & capacity Ricker
ll5<- stan_lfo_cv(mod=m5,type='tv',df=SR_data,L=15)

#model 6 - productivity regime shift - 2 regimes
ll6<- stan_lfo_cv(mod=m6,type='regime',df=SR_data,L=15,K=2)

#model 7 - capacity regime shift
ll7<- stan_lfo_cv(mod=m7,type='regime',df=SR_data,L=15,K=2)

#model 8 - productivity and capacity regime shift
ll8_1<- stan_lfo_cv(mod=m8_1,type='regime',df=SR_data,L=15,K=2)
#model 8 - productivity and capacity regime shift
ll8_2<- stan_lfo_cv(mod=m8_2,type='regime',df=SR_data,L=15,K=2)