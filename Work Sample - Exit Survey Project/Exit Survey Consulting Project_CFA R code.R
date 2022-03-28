

## install "lavaan" & "semPlot" package

install.packages("lavaan")                             
install.packages("semPlot")                            

## load "lavaan" & "semPlot"

library(lavaan)                                        
library(semPlot)                                                 

dat1 = read.csv("Mathnasium Exit Survey_updated.csv", fileEncoding="UTF-8-BOM")               ## Import data

summary(dat1)


## Model

model1 <- '
          Schedule =~ S1 + S2 + S3 + S4 + S5
          Pay =~ P1 + P2 + P3 + P4 + P5
          Training =~ T1 + T2 + T3 + T4 + T5
          Procedures =~ Pr1 + Pr2 + Pr3 + Pr4 + Pr5
          Feedback =~ F1 + F2 + F3 + F4 + F5
          '

## Calculation of fit indices & parameter estimates

fit1 <- cfa(model1, data=dat1)

## check results

summary(fit1, fit.measures=TRUE, standardized=TRUE)

## Plot results

semPaths(fit1, what="stand",style="lisrel")

## if you want to check modification indices (suggestion from R to improve the model)

mi <- modindices(fit1)

mi   # To check modification indices