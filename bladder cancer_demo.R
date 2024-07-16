#-----------------(1. packages)---------------------
library(glmnet)
library(readxl)
library(tidyverse)
library(ggplot2)
setwd("D:/R_code for 2024/Bladder cacner_20240712/paired_bladder_2022")
#-----------------(2.input data)--------------------
# resource: https://www.cbioportal.org/
bc_patient<-read.delim("data_clinical_patient.txt")
bc_samples<-read.delim("data_clinical_sample.txt")

# 
bc_01<-merge(bc_patient,bc_samples,by.x = "X.Patient.Identifier",by.y = "Patient.Identifier" )
bc_02<-bc_01[!duplicated(bc_01$X.Patient.Identifier),]
bc_03<-bc_02[,c(1:7,11,13,17,23,26,31)]

#-----------------(3.data cleaning)---------------------------------------------
#race
bc_03[bc_03$Race.Category =="ASIAN-FAR EAST/INDIAN SUBCONT", c("Race_01")] <- 0 #others
bc_03[bc_03$Race.Category  =="NO VALUE ENTERED", c("Race_01")] <- 0 #others
bc_03[bc_03$Race.Category  =="OTHER", c("Race_01")] <- 0 #others
bc_03[bc_03$Race.Category  =="PT REFUSED TO ANSWER", c("Race_01")] <- 0 #others
bc_03[bc_03$Race.Category =="RACE", c("Race_01")] <- 0 #others
bc_03[bc_03$Race.Category =="UNKNOWN", c("Race_01")] <- 0 #others
bc_03[bc_03$Race.Category =="ASIAN-FAR EAST/INDIAN SUBCONT", c("Race_01")] <- 1
bc_03[bc_03$Race.Category  =="BLACK OR AFRICAN AMERICAN", c("Race_01")] <- 2
bc_03[bc_03$Race.Category  =="NATIVE AMERICAN-AM IND/ALASKA", c("Race_01")] <- 3
bc_03[bc_03$Race.Category  =="ASIAN-FAR EAST/INDIAN SUBCONT", c("Race_01")] <- 4
bc_03[bc_03$Race.Category =="WHITE", c("Race_01")] <- 5

#sex
bc_03[bc_03$Sex =="Male", c("sex_01")] <- 1
bc_03[bc_03$Sex =="Female", c("sex_01")] <- 0

#Ethnicity.Category
bc_03[bc_03$Ethnicity.Category =="", c("Ethnicity_01")] <- 0
bc_03[bc_03$Ethnicity.Category =="Cuban", c("Ethnicity_01")] <- 1
bc_03[bc_03$Ethnicity.Category =="ETHNICITY", c("Ethnicity_01")] <- 2
bc_03[bc_03$Ethnicity.Category =="Mexican (includes Chicano)", c("Ethnicity_01")] <- 3
bc_03[bc_03$Ethnicity.Category =="Non-Spanish; Non-Hispanic", c("Ethnicity_01")] <- 4
bc_03[bc_03$Ethnicity.Category =="Puerto Rican", c("Ethnicity_01")] <- 5
bc_03[bc_03$Ethnicity.Category =="South/Central America (except Brazil)", c("Ethnicity_01")] <- 6
bc_03[bc_03$Ethnicity.Category =="Spanish  NOS; Hispanic NOS, Latino NOS", c("Ethnicity_01")] <- 7
bc_03[bc_03$Ethnicity.Category =="Unknown whether Spanish or not", c("Ethnicity_01")] <- 8

#overall survival status
bc_03[bc_03$Overall.Survival.Status =="", c("OS.status_01")] <- NA
bc_03[bc_03$Overall.Survival.Status =="LIVING", c("OS.status_01")] <- 0
bc_03[bc_03$Overall.Survival.Status =="DECEASED", c("OS.status_01")] <- 1

#overall survival time
bc_03$OS_time<-as.numeric(bc_03$Overall.Survival..Months.)

#smoker
bc_03[bc_03$Smoker=="Never", c("smoker_01")] <- 0
bc_03[bc_03$Smoker=="Former", c("smoker_01")] <- 1
bc_03[bc_03$Smoker=="Active", c("smoker_01")] <- 2

#age at diagnosis
bc_03$age_01<-as.numeric(bc_03$Age.at.Diagnosis)

#MSI score
bc_03$MSI_01<-as.numeric(bc_03$MSI.Score)

#TMB
bc_03$TMB_01<-as.numeric(bc_03$TMB..nonsynonymous.)

#final
bc_04<-bc_03[,c(1,14:22)]

#delete na
bc_05<-na.omit(bc_04)

#-----------------(4.descriptive analysis)
#-----------------(5.simple logistic regression)--------------------------------
#what is the different risk between female and male in the bladder cancer?
s_glm = glm(sex_01 ~ smoker_01, data = bc_05, family = "binomial")


#-----------------(6.prediction models)-----------------------------------------
#what is the best clinical model ? Ans:ROC prediction for logistic regression
vari<-data.matrix(bc_05) 
fit<-glmnet(vari[,c(2,3,6:10)],vari[,5],family ="binomial",alpha = 1)

#how many variables are selected to make prediction model ?
set.seed(166)
cvfit <- cv.glmnet(vari[,c(2,3,6:10)],vari[,5], family = "binomial", type.measure ="auc",alpha = 1,keep=TRUE, nfolds=10)
coef(cvfit)

#ROC prediction for logistic regression
cfit <- cv.glmnet(vari[,c(2,3,6:10)],vari[,5],family ="binomial", type.measure = "auc",alpha = 1,keep=TRUE, nfolds=10)
rocs <- roc.glmnet(cfit$fit.preval, newy =vari[,5])
best <- cvfit$index["min",]
plot(rocs[[best]], type = "l")
invisible(sapply(rocs, lines, col="grey"))
lines(rocs[[best]], lwd = 2,col = "red")












