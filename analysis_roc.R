library(ggplot2)
library(plyr)
library(pROC)
library(doMC)

# how many cores to use to run bootstrap simulation
doMC::registerDoMC(cores=4) # or however many cores you have access to

# all crimes data
raw_data <- read.csv("compas-scores-two-years.csv")

# filter out some data as in original study
all_data <- raw_data[!is.na(raw_data$days_b_screening_arrest) &
                (raw_data$days_b_screening_arrest <= 30) &
                (raw_data$days_b_screening_arrest >= -30) &
                (!is.na(raw_data$is_recid)) &
                (raw_data$is_recid != -1) &
                (!is.na(raw_data$c_charge_degree)) &
                (raw_data$c_charge_degree != "O") &
                (raw_data$score_text != 'N/A') &
                (!is.na(raw_data$score_text)), ]


# violent crimes data
raw_data <- read.csv("./compas-scores-two-years-violent.csv")

violent_data <- raw_data[!is.na(raw_data$days_b_screening_arrest) &
                (raw_data$days_b_screening_arrest <= 30) &
                (raw_data$days_b_screening_arrest >= -30) &
                (!is.na(raw_data$is_recid)) &
                (raw_data$is_recid != -1) &
                (!is.na(raw_data$c_charge_degree)) &
                (raw_data$c_charge_degree != "O") &
                (raw_data$score_text != 'N/A') &
                (!is.na(raw_data$score_text)), ]

#data$decile_score<-as.numeric(data$decile_score)
all_data$two_year_recid<-all_data$two_year_recid==1
violent_data$two_year_recid<-violent_data$two_year_recid==1

races=c("African-American","Caucasian","Hispanic","All")
types=c('All Crimes','Violent Crimes')
lcv  <- vector(mode = "list", length = length(races)*2)
lcv2 <- vector(mode = "list", length = length(races)*2)

for(t in seq(length(types)) ) {
  d<-all_data
  if(types[t]=='Violent Crimes') {
    d<-violent_data
  }
for(l in seq(length(races)) ) {
  
  ss_all<-d
  if(races[l]!="All") {
    ss_all<-d[d$race==races[l],]
  }
  sp<-seq(0,1,0.1)
  # run ROC analysis  
  r<-roc(ss_all$two_year_recid,ss_all$decile_score,specificities=sp) 
  ci_auc=ci.auc(r)
  ci_se=ci.se(r,specificities=sp, parallel=T)
  lcv2[[ l+(t-1)*length(races) ]] <- data.frame(auc=sprintf("AUC=%0.3f\nCI:%0.3f - %0.3f", ci_auc[2],ci_auc[1],ci_auc[3]), race=races[l], sp=0.6, se=0.1, type=types[t],se_low=0,se_hi=0 ) 
  lcv[[ l+(t-1)*length(races) ]]  <- data.frame(sp=sp, se=ci_se[,2],se_low=ci_se[,1],se_hi=ci_se[,3], race=rep(races[l],length(r$se)), type=rep(types[t],length(r$se)) )
  
}
}

g_roc<-rbind.fill(lcv)
g_roc_t<-rbind.fill(lcv2)

png('compas_roc.png',width=800,height=400)

ggplot(data=g_roc,aes(y=se,x=1-sp,ymin=se_low,ymax=se_hi,col=race,fill=race))+
  geom_line()+coord_fixed()+
  geom_ribbon(lty=3,alpha=0.2)+
  xlab('1-Specificity')+ylab('Sensitivity')+
  geom_abline(slope=1.0,intercept=0.0,lty=2,col='black',alpha=0.5)+
  facet_grid(type~race)+
  ggtitle('decile score ROC curves')+
  geom_text(data=g_roc_t,aes(y=se,x=1-sp,label=auc),size=5,col='black')+
 theme_bw()+
 theme(
   axis.text  = element_text(vjust = 0.2, size = 12),
   axis.title = element_text(face = 'bold', vjust = 0.2, size = 12),
   plot.title = element_text(face = 'bold', vjust = 2.0, size = 15),
   strip.text = element_text(face = 'bold', size = 12),
   plot.margin = unit(c(1.0,0.2,0.2,0.2), "cm"),
   legend.position="none"
   )
