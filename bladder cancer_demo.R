#-----------------(1. packages)---------------------
library(glmnet)
library(readxl)
library(tidyverse)
library(ggplot2)
#-----------------(2.input data)--------------------
Resource: https://www.cbioportal.org/
Type_of_cancer: bladder
Cancer_study_identifier: paired_bladder_2022
Name: Bladder Cancer (MSK, Cell Reports 2022)
Description: Targeted sequencing of 1659 Bladder Cancer tumor/normal samples via MSK-IMPACT.
Short_name: BLADDER (MSK 2022)
#-----------------(3.simple logistic regression)--------------------------------
#what is the different risk between female and male in the bladder cancer?
s_glm = glm(sex_01 ~ smoker_01, data = bc_05, family = "binomial")

#-----------------(4.prediction models)-----------------------------------------
#Reference: https://glmnet.stanford.edu/articles/glmnet.html
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












