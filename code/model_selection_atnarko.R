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

#Final model fits
m2f=sr_mod(type='static',ac = TRUE,par='n',loglik=F)
m7f=sr_mod(type='hmm',par='b',loglik=F)
m6f=sr_mod(type='hmm',par='a',loglik=F)
m2_cov=sr_mod(type='static',ac = TRUE,par='n',loglik=F,cov=T)

fit_2f= rstan::sampling(m2f, 
                        data = list(N=nrow(SR_data),
                                    L=max(SR_data$broodyear)-min(SR_data$broodyear)+1,
                                    ii=SR_data$broodyear-min(SR_data$broodyear)+1,
                                    R_S =SR_data$logR_S,
                                    S=SR_data$spawners
                        ),
                        control = list(adapt_delta = 0.95,max_treedepth=20), warmup = 200, chains = 6, iter = 700)

X= matrix(data=c(SR_data2$cm_fry_lag,SR_data2$pk_avg,SR_data2$SST_pine_yr,SR_data2$NPGO_2wint),ncol=4,nrow=nrow(SR_data2))

fit_2cov= rstan::sampling(m2_cov, 
                          data = list(N=nrow(SR_data),
                                      L=max(SR_data$broodyear)-min(SR_data$broodyear)+1,
                                      ii=SR_data$broodyear-min(SR_data$broodyear)+1,
                                      R_S =SR_data$logR_S,
                                      S=SR_data$spawners,
                                      X=X,
                                      Z=ncol(X)),
                          control = list(adapt_delta = 0.95,max_treedepth=20), warmup = 200, chains = 6, iter = 700)


fit_6f= rstan::sampling(m6f, 
                        data = list(N=nrow(SR_data),
                                    L=max(SR_data$broodyear)-min(SR_data$broodyear)+1,
                                    ii=SR_data$broodyear-min(SR_data$broodyear)+1,
                                    R_S =SR_data$logR_S,
                                    S=SR_data$spawners,
                                    K=2,
                                    alpha_dirichlet=c(1,1)),
                        control = list(adapt_delta = 0.95,max_treedepth=20), warmup = 200, chains = 6, iter = 700)

fit_7f= rstan::sampling(m7f, 
                        data = list(N=nrow(SR_data),
                                    L=max(SR_data$broodyear)-min(SR_data$broodyear)+1,
                                    ii=SR_data$broodyear-min(SR_data$broodyear)+1,
                                    R_S =SR_data$logR_S,
                                    S=SR_data$spawners,
                                    K=2,
                                    alpha_dirichlet=c(1,1)),
                        control = list(adapt_delta = 0.95,max_treedepth=20), warmup = 200, chains = 6, iter = 700)


d2=extract(fit_2f)
d6=extract(fit_6f)
d7=extract(fit_7f)

save.image(here('data','loglik.RData'))
