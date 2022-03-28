## Load packages ##

library(psych)                                                                                     
library(GPArotation)
library(dplyr)
library(rela)
library(Rcpp)
library(EFAtools)
library(formattable)
library(DT)

## Load dataset ##


dat1 = read.csv(file.choose(), header=T)                ## Import data

## Reverse code ##

dat1[,c(1,7,9,10,13,18,19,25,26,28,31)] <- 6-dat1[,c(1,7,9,10,13,18,19,25,26,28,31)]

View(dat1)

## Step 1: Calculate mean and SD of each item ##

dat1 %>% summarise_if(is.numeric, sd)                   ## Calculate SD for each item

colMeans(dat1)                                          ## Calculate mean for each item

## Step 2: Conduct EFA to examine the dimensionality ##

#### KMO & Bartlett test (or RMSEA)  #####

KMO(dat1)                                               ## Calculate KMO with EFAtools package

BARTLETT(dat1)                                          ## Conduct Bartlett test with EFAtools package

#### Eigenvalue & Screeplot  #####

cor1 <- cor(dat1)   ## Correlation for eigenvalue

fa.parallel(cor1, n.obs = sum(complete.cases(dat1)), fa = "fa" )     ## Parallel Analysis & Screeplot 
abline(h = 1)                                                        ## line: Eigenvalue =1


## If you want to create a big scree plot ## 

eigen1 <- eigen(cor1)$values                                                   ## Check eigenvalues
plot(eigen1, type="b", main="Scree Plot",xlab="Number", ylab="Eigenvalue")     ## Scree plot


## other methods ## 

VSS(dat1, n=8)                       ## Use MAP or BIC (Bayesian information criterion)
                                     ## Number of factors to extract -- should be more than hypothesized!

## Rotation  ###

fit1 <- fa(r=dat1, nfactors=2, rotate="promax", fm="pa")     ## FA using principal axis factoring, promax

print(fit1, digits = 3, sort = T)                            ## See results:should not forget to add "sort=T"!!

print(fit1$loadings, digits = 3, cutoff = 0.3 )              ## See only factor loadings & add cutoff score 

fa.diagram(fit1)                                         ## if you want to depict figures

result1 <-fit1$loadings                                  ## select only the result of factor loadings

write.csv(result1,"(reverse coded) PA4_EFA_Results (2 factors).csv")   ## Export the results

## Export communality, uniquenesses, complexity ##

result_h<-fit1$communality
result_u<-fit1$uniquenesses
result_com<-fit1$complexity

result3 <- cbind(result_h, result_u, result_com)     ## merge the results
write.csv(result3,"(reverse coded) PA4_EFA_Commuality_Uniquenesses_Complexity (2 factors).csv")   ## Export the result

## update dataset with 10 items in each dimension ##

dat2 <- select(dat1, 1,2,8,11:17,20,22,24,26:28,30:33)  ## create a new data set with only the selected items

head(dat2)                                                   ## double-check if the items are the correct ones 

## compute Omega based on EFA ##

columnList1 <- c("I1", "I2", "I8", "I11", "I12", "I13", "I14", "I15", "I16", "I17")  ## create lists of names for the columns 
columnList2 <- c("St2", "St4", "St6", "St8", "St9", "St10", "St12", "St13", "St14", "St15")

## calculation Cronbach's alpha

alpha(dat1[, columnList1])  
alpha(dat1[, columnList2])

## calculation McDonalds' Omega                

omega(dat1[, columnList1])                
omega(dat1[, columnList2])


