data<-fread("/Users/victornauffal/Dropbox (Partners HealthCare)/BST210 Group Project/Data/cleaned_dataset_long_format.csv")
names(data)[grep("icd",names(data))]
table(data[data$replicate==1,"medicalhx_icd_now"])
test<-data %>% filter(replicate==1 & medicalhx_icd_now=="Yes")
data<-data[!(data$pid%in%test$pid),]###Excluding those who had an ICD implanted prior to enrollment in study
nrow(data)
table(data[data$replicate==1,"medicalhx_icd_now"])

data<-data %>% filter(!(medicalhx_icd_indication_ever=="Secondary") | is.na(medicalhx_icd_indication_ever))###Excluding Secondary Indication ICDs
table(data$medicalhx_icd_ever,data$medicalhx_icd_indication_ever,useNA="always")

table(data$medicalhx_icd_ever,data$medicalhx_icd,useNA="always")
data<-data %>% filter(medicalhx_icd==1 | medicalhx_icd_ever=="No")##Including only visit at ICD implant for those with ICD
table(data$medicalhx_icd_ever,data$medicalhx_icd,useNA="always")

data<-data[order(data$pid, -data$age),]
head(data,20)
data<-data %>% group_by(pid) %>% mutate(replicate=seq(n()))
head(data[,c("pid","age","replicate")])
table(data$replicate,data$medicalhx_icd,useNA="always")
data<-data %>% filter(replicate==1)##Including only last follow-up visit for individuals without ICD implant
write.csv(data,file="/Users/victornauffal/Dropbox (Partners HealthCare)/BST210 Group Project/Data/collapsed_file_with_single_visit_and_exclusion_criteria_applied.csv", sep="")
