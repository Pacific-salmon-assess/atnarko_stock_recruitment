
pdf(here('figures','Figure 1 - SR.pdf'),width=8,height=6)
ggplot(SR_data, aes(spawners, recruits)) +
  geom_point(aes(colour = broodyear),size=2.5) +
  scale_colour_viridis_c(name='Year')+xlab("Spawners") + ylab("Recruits")+theme(axis.text=element_text(size=11),
                                                                                axis.title=element_text(size=12,face="bold"))
dev.off()


pdf(here('figures','Figure 2a - SR static.pdf'),width=8,height=6)
ggplot(SR_data, aes(spawners, recruits)) +
  geom_point(aes(colour = broodyear),size=2.5,) +
  scale_colour_viridis_c(name = "Year")+geom_line(data=pred_df,aes(x=x_new,y=y_pred_d2))+xlab("Spawners") + ylab("Recruits")+theme(axis.text=element_text(size=11),
                                                                                                                                   axis.title=element_text(size=12,face="bold"))
dev.off()

pdf(here('figures','Figure 2a_2 - autocorrelation.pdf'),width=8,height=6)
hist(d2$rho,xlab='Autocorrelation coefficient',main='',bins=30,xlim=c(0,1))
dev.off()

pdf(here('figures','Figure 2b - SR cap regime.pdf'),width=8,height=6)
ggplot(SR_data, aes(spawners, recruits)) +
  geom_point(aes(colour = broodyear),size=2.5) +
  scale_colour_viridis_c(name = "Year")+geom_line(data=pred_df,aes(x=x_new,y=y_pred_d7.1))+geom_line(data=pred_df,aes(x=x_new,y=y_pred_d7.2))+xlab("Spawners") + ylab("Recruits")+theme(axis.text=element_text(size=11),
                                                                                                                                                                                     axis.title=element_text(size=12,face="bold"))
dev.off()   


pdf(here('figures','Figure 2c - SR prod regime.pdf'),width=8,height=6)
ggplot(SR_data, aes(spawners, recruits)) +
  geom_point(aes(colour = broodyear),size=2.5) +
  scale_colour_viridis_c()+geom_line(data=pred_df,aes(x=x_new,y=y_pred_d6.1))+geom_line(data=pred_df,aes(x=x_new,y=y_pred_d6.2))+theme(axis.text=element_text(size=11),
                                                                                                                                       axis.title=element_text(size=12,face="bold"))

dev.off()

pdf(here('figures','Figure 3a - SR cap regime.pdf'),width=8,height=6)
ggplot(SR_data, aes(x=spawners,y=recruits)) +
  geom_point(aes(colour = p.High_Cap),size=2.5) +
  scale_colour_viridis_c()+geom_line(data=pred_df,aes(x=x_new,y=y_pred_d7.1))+geom_line(data=pred_df,aes(x=x_new,y=y_pred_d7.2))+ geom_text(data=SR_data, aes(x=spawners, y=recruits, label=broodyear),hjust=1.2, vjust=0.8,size=2.5)+theme(axis.text=element_text(size=11),
                                                                                                                                                                                                                                            axis.title=element_text(size=12,face="bold"))

dev.off()

pdf(here('figures','Figure 3b - SR prod regime.pdf'),width=8,height=6)
ggplot(SR_data, aes(x=spawners,y=recruits)) +
  geom_point(aes(colour = p.High_Prod),size=2.5) +
  scale_colour_viridis_c()+geom_line(data=pred_df,aes(x=x_new,y=y_pred_d6.1))+geom_line(data=pred_df,aes(x=x_new,y=y_pred_d7.1))+ geom_text(data=SR_data, aes(x=spawners, y=recruits, label=broodyear),hjust=1.2, vjust=0.8,size=2.5)+theme(axis.text=element_text(size=11),
                                                                                                                                                                                                                                            axis.title=element_text(size=12,face="bold"))

dev.off()

pdf(here('figures','Figure 4a - cap regime probability timeseries.pdf'),width=8,height=6)
ggplot(gamma_d_m7, aes(by, prob)) +
  geom_point(aes(colour = prob),size=3) +
  scale_colour_viridis_c() + geom_ribbon(aes(ymin =l90, ymax =u90), alpha = 0.2) +xlab('Brood year')+ylab('Prob. of High Capacity Regime') + geom_hline(yintercept=0.5)+theme(axis.text=element_text(size=11),
                                                                                                                                                                              axis.title=element_text(size=12,face="bold"))

dev.off()

pdf(here('figures','Figure 4b - prod regime probability timeseries.pdf'),width=8,height=6)
ggplot(gamma_d_m6, aes(by, prob)) +
  geom_point(aes(colour = prob),size=3) +
  scale_colour_viridis_c() + geom_ribbon(aes(ymin =l90, ymax =u90), alpha = 0.2) +xlab('Brood year')+ylab('Prob. of High Productivity Regime') + geom_hline(yintercept=0.5)+theme(axis.text=element_text(size=11),
                                                                                                                                                                                  axis.title=element_text(size=12,face="bold"))

dev.off()

pdf(here('figures','Figure 5a - Smax static.pdf'),width=8,height=6)
hist(d2$S_max[(d2$S_max<quantile(d2$S_max,0.9))],main='S_max',xlab='spawners')
dev.off()

pdf(here('figures','Figure 5b - Smax cap regime.pdf'),width=8,height=6)
par(mfrow=c(1,2))
hist(d7$S_max[(d7$S_max[,1]<quantile(d7$S_max[,1],0.9)),1],main='S_max, High Capacity Regime',xlab='spawners');hist(d7$S_max[(d7$S_max[,2]<quantile(d7$S_max[,2],0.9)),2],main='S_max, Low Capacity Regime',xlab='spawners')
dev.off()

pdf(here('figures','Figure 5c - Smax prod regime.pdf'),width=8,height=6)
hist(d6$S_max[(d6$S_max<quantile(d6$S_max,0.9))],main='S_max',xlab='spawners')
dev.off()

pdf(here('figures','Figure 6a - Smsy static.pdf'),width=8,height=6)
hist(d2$S_msy[(d2$S_msy<quantile(d2$S_msy,0.9))],main='S_msy, Autocorrelated Ricker',xlab='spawners')
dev.off()

pdf(here('figures','Figure 6b - Smsy cap regime.pdf'),width=8,height=6)
par(mfrow=c(1,2))
hist(d7$S_msy[(d7$S_msy[,1]<quantile(d7$S_msy[,1],0.9)),1],main='S_msy, High Capacity Regime',xlab='spawners')
hist(d7$S_msy[(d7$S_msy[,2]<quantile(d7$S_msy[,2],0.9)),2],main='S_msy, Low Capacity Regime',xlab='spawners')
dev.off()

pdf(here('figures','Figure 6c - Smsy prod regime.pdf'),width=8,height=6)
par(mfrow=c(1,2))
hist(d6$S_msy[(d6$S_msy[,2]<quantile(d6$S_msy[,2],0.9)),2],main='S_msy, High Productivity Regime',xlab='spawners')
dev.off()

pdf(here('figures','Figure 7a - Umsy static.pdf'),width=8,height=6)
hist(d2$U_msy,main='U_msy',xlab='exploitation rate',xlim=c(0,1))
dev.off()

pdf(here('figures','Figure 7b - Umsy cap regimes.pdf'),width=8,height=6)
hist(d6$U_msy,main='U_msy',xlab='exploitation rate',xlim=c(0,1))
dev.off()

pdf(here('figures','Figure 7c - Umsy prod regimes.pdf'),width=8,height=6)
par(mfrow=c(1,2))
hist(d6$U_msy[,2],main='U_msy, High Productivity Regime',xlab='exploitation rate',xlim=c(0,1))
hist(d6$U_msy[,1],main='U_msy, Low Productivity Regime',xlab='exploitation rate',xlim=c(0,1))
dev.off()

pdf(here('figures','Figure 8 - prod residuals time-series.pdf'),width=8,height=6)
ggplot(resid.df, aes(by, resids)) +
  geom_hline(yintercept=0)+geom_line(aes(by,resids))+
  geom_point(aes(colour = by),size=3) +
  scale_colour_viridis_c(name = "Year",) + geom_ribbon(aes(ymin =resid.l90, ymax=resid.u90), alpha = 0.2) +xlab('Brood year')+ylab('Annual Productivity Deviation')+theme(axis.text=element_text(size=11),
                                                                                                                                                                                     axis.title=element_text(size=12,face="bold"))
dev.off()

pdf(here('figures','Figure 9 - covariate time-series.pdf'),width=16,height=14)
plot_grid(tp9,legend,rel_widths = c(3,.3))
dev.off()

pdf(here('figures','Figure 10 - residuals vs covariates.pdf'),width=16,height=14)
plot_grid(tp10,legend,rel_widths = c(3,.3))
dev.off()

pdf(here('figures','Figure 11 - effect sizes.pdf'),width=16,height=16)
plot_grid(f11a,f11b,f11c,f11d,ncol=2,nrow=2)
dev.off()
