setwd('/Users/yixuanhe/Dropbox (HMS)/BST210 Group Project/Data/')
df<-fread('collapsed_file_with_single_visit_and_exclusion_criteria_applied.csv')
extract<-c('pid','medicalhx_icd_now','isus','age_cat','age','white','demographics_sex',
           'medicalhx_afib_now','medicalhx_cad_now','medicalhx_htn_now',
           'medicalhx_diabetes_now','medicalhx_scd_family_hx', 'medicalhx_nsvt_now',
           'medicalhx_syncope_now','medicalhx_septal_reduction_therapy_now',
           'medicalhx_etoh_ablation_now','sarc_plp','edicalhx_pacer_now_hx',
           'eventyear', 'esc_riskscore',
           'echo_lvef', 'echo_la','echo_max_lv_thickness','max_lvoto'
)
tab<-df[,which(colnames(df)%in%extract),with=F]

##making table
colnames(tab)<-c('PID','Age','ESC risk score','Site (US/non-US)','Sex','Echo (LA)','Echo (LVEF)',
                 'Echo (max LV thickness)','FHX Sudden Cardiac Death','HX Atrial Fibrillation',
                 'HX CAD','HX Diabetes','HX Alcohol Ablation','HX Hypertension', 'ICD',
                 'HX Non-Sustained Ventricular Tachycardia','HX Septal Reduction Therapy',
                 'HX Syncope','Event year of event','Age Category','Race (White/non-White)',
                 'Max LVOT', 'Genetic status')
col.order<-c('PID','ICD','Site (US/non-US)','Race (White/non-White)','Sex','Age','Age Category',
            'Event year of event','Genetic status', 'ESC risk score',
            'FHX Sudden Cardiac Death','HX Atrial Fibrillation',
            'HX CAD','HX Diabetes','HX Alcohol Ablation','HX Hypertension', 
            'HX Non-Sustained Ventricular Tachycardia','HX Septal Reduction Therapy',
            'HX Syncope',
            'Max LVOT','Echo (LA)','Echo (LVEF)',
            'Echo (max LV thickness)')
tab<-tab[,col.order,with=F]

M<-data.table(colnames(tab))
for (l in c('Site (US/non-US)','Race (White/non-White)','Sex','Age Category',
            'Event year of event','Genetic status', 'ESC risk score',
            'FHX Sudden Cardiac Death','HX Atrial Fibrillation',
            'HX CAD','HX Diabetes','HX Alcohol Ablation','HX Hypertension', 
            'HX Non-Sustained Ventricular Tachycardia','HX Septal Reduction Therapy',
            'HX Syncope') ){
  temp<-tab[,which(colnames(tab)==l),with=F]
  temp<-data.frame(table(temp))
  temp$Freq=temp$Freq/sum(temp$Freq)
  print(l)
  print(temp)
}
write.csv(M,'/Users/yixuanhe/Dropbox (HMS)/BST210 Group Project/Tables/Table_1.csv',row.names=F)

##univariate 

y=as.numeric(as.factor(tab$ICD))-1
tab<-data.frame(tab)
ma<-data.frame(matrix(0,nrow=ncol(tab),ncol=3))
colnames(ma)<-c('X','estimate','p')
for (i in c(3:ncol(tab))){
  x<-tab[,i]
  temp<-summary(glm(y~x,family='binomial'))
  ma$X[i]<-as.character(colnames(tab)[i])
  ma$estimate[i]<-coefficients(temp)[2,1]
  ma$p[i]<-coefficients(temp)[2,4]
  print(i)
}
ma<-ma[-(1:2),]
ma$X<-col.order[-(1:2)]
ma$fdr<-p.adjust(ma$p,method='fdr')
write.csv(ma,'/Users/yixuanhe/Dropbox (HMS)/BST210 Group Project/Tables/univariate_logistic.csv',row.names=F)

##multinomial
tab$ICD<-as.numeric(as.factor(tab$ICD))-1
tab$PID<-NULL
sum<-summary(glm(ICD~.,data=tab))
write.csv(sum$coefficients,'/Users/yixuanhe/Dropbox (HMS)/BST210 Group Project/Tables/multinomial_logistic.csv')

##lasso
library(glmnet)
# Find the best lambda using cross-validation
set.seed(7) 
tab_lasso<-na.omit(tab)
y=as.numeric(as.factor(tab_lasso$ICD))-1
x=model.matrix(ICD~., tab_lasso)[,-14]
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
# Fit the final model on the training data
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
# Display regression coefficients
coef(model)
#

write.csv(as.matrix(coef(model)),'/Users/yixuanhe/Dropbox (HMS)/BST210 Group Project/Tables/lasso_coeffs.csv')



##missingness:

echos<-tab[,c(1,20:23)]
echos<-na.omit(echos)
complete<-tab[which(tab$PID%in%echos$PID),]
for (i in c(2:5,7,9:19)){
  print(colnames(complete)[i])
  print(table(complete[,i,with=F]))
}

for (i in c(6,8,20:23)){
  print(colnames(complete)[i])
  print(summary(complete[,i,with=F]))
  print(sd(data.frame(complete[,i,with=F])[,1]))
}
