library(ggplot2)
library(plyr)
library(pROC)

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

lcv  <- vector(mode = "list", length = length(races)*2)
lcv2 <- vector(mode = "list", length = length(races)*2)

for(l in seq(length(races)) ) {
  
  ss_all<-all_data
  if(races[l]!="All") {
    ss_all<-all_data[all_data$race==races[l],]
  }
  ss_violent<-violent_data
  if(races[l]!="All") {
    ss_violent<-violent_data[violent_data$race==races[l],]
  }
  
  # run ROC analysis  
  r_all<-roc(ss_all$two_year_recid,ss_all$decile_score) 
  lcv2[[ l ]] <- data.frame(auc=sprintf("AUC=%0.3f\nCI:%0.3f - %0.3f", auc(r_all),ci.auc(r_all)[1],ci.auc(r_all)[3]), race=races[l], specificities=0.6, sensitivities=0.1, type='All Crimes' ) 
  lcv[[ l ]]  <- data.frame(specificities=r_all$specificities, sensitivities=r_all$sensitivities,  race=rep(races[l],length(r_all$sensitivities)), type=rep('All Crimes',length(r_all$sensitivities)) )
  
  r_violent<-roc(ss_violent$two_year_recid,ss_violent$decile_score) 
  #r_violent_ci=ci.sp(r_violent)
  
  lcv2[[ l+length(races) ]] <- data.frame(auc=sprintf("AUC=%0.3f\nCI:%0.3f - %0.3f", auc(r_violent),ci.auc(r_all)[1],ci.auc(r_all)[3]), race=races[l], specificities=0.6, sensitivities=0.1, type='Violent Crimes' ) 
  lcv[[  l+length(races) ]] <- data.frame(specificities=r_violent$specificities, sensitivities=r_violent$sensitivities,  race=rep(races[l],length(r_violent$sensitivities)), type=rep('Violent Crimes' ,length(r_violent$sensitivities)) )
}

g_roc<-rbind.fill(lcv)
g_roc_t<-rbind.fill(lcv2)

png('compas_roc.png',width=800,height=400)

ggplot(data=g_roc,aes(y=sensitivities,x=1-specificities))+
  geom_line()+coord_fixed()+
  xlab('1-Specificity')+ylab('Sensitivity')+
  geom_abline(slope=1.0,intercept=0.0,lty=2,col='red')+
  facet_grid(type~race)+
  ggtitle('decile score ROC curves')+
  geom_text(data=g_roc_t,aes(y=sensitivities,x=1-specificities,label=auc),size=5)+
 theme_bw()+
 theme(
   axis.text  = element_text(vjust = 0.2, size = 12),
   axis.title = element_text(face = 'bold', vjust = 0.2, size = 12),
   plot.title = element_text(face = 'bold', vjust = 2.0, size = 15),
   strip.text = element_text(face = 'bold', size = 12),
   plot.margin = unit(c(1.0,0.2,0.2,0.2), "cm")
   )
