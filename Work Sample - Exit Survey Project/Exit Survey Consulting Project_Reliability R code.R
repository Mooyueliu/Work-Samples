library(psych)                                                                                     
library(GPArotation)
library(lavaan)
library(sjPlot)

## Omega based on CFA ##

dat1 = read.csv(file.choose(), header=T) 

dat1

columnList1 <- c("S1", "S2", "S3", "S4", "S5")
columnList2 <- c("P1", "P2", "P3", "P4", "P5")
columnList3 <- c("T1", "T2", "T3", "T4", "T5")
columnList4 <- c("Pr1", "Pr2", "Pr3", "Pr4", "Pr5")
columnList5 <- c("F1", "F2", "F3", "F4", "F5")

omegaSem (dat1[, columnList1])
omegaSem (dat1[, columnList2])
omegaSem (dat1[, columnList3])
omegaSem (dat1[, columnList4])
omegaSem (dat1[, columnList5])

## Alpha & Omega based on EFA

## calculation Cronbach's alpha

alpha(dat1[, columnList1])                 ## Scheduling
alpha(dat1[, columnList2])                 ## Pay
alpha(dat1[, columnList3])                 ## Training
alpha(dat1[, columnList4])                 ## Procedures
alpha(dat1[, columnList5])                 ## Feedback


## calculation McDonald's Omega                

omega(dat1[, columnList1])                ## Scheduling
omega(dat1[, columnList2])                ## Pay
omega(dat1[, columnList3])                ## Training
omega(dat1[, columnList4])                ## Procedures
omega(dat1[, columnList5])                ## Feedback

## Correlation test

corr.test(dat1[, columnList1])            ## Scheduling
corr.test(dat1[, columnList2])            ## Pay
corr.test(dat1[, columnList3])            ## Training
corr.test(dat1[, columnList4])            ## Procedures
corr.test(dat1[, columnList5])            ## Feedback

## Correlation test but prettier

sjp.corr(dat1[, columnList1])             ## Scheduling
sjp.corr(dat1[, columnList2])             ## Pay
sjp.corr(dat1[, columnList3])             ## Training
sjp.corr(dat1[, columnList4])             ## Procedures
sjp.corr(dat1[, columnList5])             ## Feedback

