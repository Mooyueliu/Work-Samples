## Lab 1 - IRT 2PL & GRM ##



### Task 1: IRT 2PL ###

## install "ltm" package

install.packages("ltm")     

## load "ltm" and "psych"

library(ltm)
library(psych)
library(tidyverse)                                      ## Load if you need to reverse items
library(formattable)
library(irtoys)

dat1 = read.csv(file.choose(), header=T)                ## Import data



## Basic statistics using ltm
## Focus on percentages of correct answers by questions but you can confirm Point Biserial correlation with Total Score and alpha too

descript(dat1)    


## Examine uni-diemnsionality

polychoric.cor <- polychoric(dat1)           ## confirm ploychoric correlation

print(polychoric.cor$rho)                    ## see the result of ploychoric correlation

eigen(polychoric.cor$rho)$value              ## To check uni-dimensionality, check eigenvalues

VSS.scree(polychoric.cor$rho,main="scree plot")    ## depict the scree plot 


## Estimation of IRT parameters with 2PL 

para_result <- ltm(dat1 ~ z1, IRT.param = TRUE)

para_result2 <- est(resp=dat1, model= "2pl", eigen ="ltm")   #### Continue working from here ####

para_result     ## To see results, write the save data name ("para_result" in this code) 

coef(para_result)   ## When you want to see only parameter estimates

summary(para_result)   ## When you want to see all indecies 

## Person Fit ##



## Item Fit ##

## Theta ##

## Depict ICC, IIC, & TIC

plot(para_result, type = "ICC", legend = TRUE)  # ICC for all items 

plot(para_result, type = "ICC", legend = TRUE, items=4) # ICC for the item 4

plot(para_result, type = "IIC", legend = TRUE)  # IIC for all items 

plot(para_result, type = "IIC", legend = TRUE, items=4) # IIC for the item 4

plot(para_result, type = "IIC", items=0)  # TIC 








### Task 2: IRT: GRM ###

dat2 = read.csv(file.choose(), header=T)                ## Import data from PA4 (pick just one dimersion)

## Basic statistics using ltm
## Focus on percentages of options

descript(dat1)    

## If there are items without any options, need to take care of them (see PPT slides)

## Examine uni-diemnsionality

cor_result <- cor(dat1, use="complete.obs")           ## confirm ploychoric correlation

eigen(cor_result)$value                               ## To check uni-dimensionality, check eigenvalues

VSS.scree(cor_result,main="scree plot")    ## depict the scree plot 

## if your data include reverse items, do reverse coding

dat1$Q14R<-6-dat1$Q14                      ## reverse coding for Q14 (it is okay to use "mutate" function)
dat1$Q44R<-6-dat1$Q44                      ## reverse coding for Q44

dat2<- select(dat1, 1, 3:6, 8:12)          ## create a new data set removing Q14 & Q44
head(dat2)                                 ## see the first 6 rows of the data


## Estimation of IRT parameters with GRM 

para_grm <- grm(dat1, IRT.para=T)   ## estimate parameters
para_grm                            ## To see results, write the save data name ("para_grm" in this code)
coef(para_grm)                      ## When you want to see only parameter estimates
summary(para_grm)                   ## When you want to see all indecies

result <- coef(para_grm)
write.csv(result,"result.csv")      ## export the result 


## Depict IRCCC

plot(para_grm, type = "ICC", legend = TRUE) # IRCCC for all items
# To see the graphs, click it
plot(para_grm, type = "ICC", legend = TRUE, items=3) # IRCCC for item 3 
