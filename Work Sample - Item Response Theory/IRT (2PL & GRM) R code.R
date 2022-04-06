### IRT 2PL & GRM ###

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
# use ltm package 

para_result <- ltm(dat1 ~ z1, IRT.param = TRUE)

para_result     ## To view results

coef(para_result)   ## When you want to see only parameter estimates

summary(para_result)   ## When you want to see all indecies 

# use irtoys package 

para_result2 <- est(resp=dat1, model="2PL", engine="ltm")
ip <- para_result2$est

describe(ip)                  ## summary of parameters

p.fit <- api(dat1, ip)        ## person fit

p.fit

which(abs(p.fit)>2.0)         ## pick up the persons which are not good fit

itf(dat1, ip, item=1, main=paste0("item 1"))   ## item fit (can check local independence
itf(dat1, ip, item=2, main=paste0("item 2")) 
itf(dat1, ip, item=3, main=paste0("item 3")) 
itf(dat1, ip, item=4, main=paste0("item 4")) 
itf(dat1, ip, item=5, main=paste0("item 5")) 
itf(dat1, ip, item=6, main=paste0("item 6")) 
itf(dat1, ip, item=7, main=paste0("item 7")) 
itf(dat1, ip, item=8, main=paste0("item 8")) 
itf(dat1, ip, item=9, main=paste0("item 9")) 
itf(dat1, ip, item=10, main=paste0("item 10")) 

## p>.05 indicates good fit

theta <- eap(resp=dat1, ip=ip, qu=normal.qu())

thetadescrip <- describe(theta [,])

formattable(thetadescrip)

write.csv(theta, "abilitylevel.csv", quote=FALSE)

## Depict ICC, IIC, & TIC

#ltm package

plot(para_result, type = "ICC", legend = TRUE)  # ICC for all items 
plot(para_result, type = "ICC", legend = TRUE, items=4) # ICC for an individual item
plot(para_result, type = "IIC", legend = TRUE)  # IIC for all items 
plot(para_result, type = "IIC", legend = TRUE, items=6) # IIC for an individual item
plot(para_result, type = "IIC", items=0)  # TIC 

#irtoys package
plot(trf(ip), co=NA, label=True)                               #ICC
plot(iif(ip), co=NA, label=True)                               #IIC
plot(tif(ip), co=NA, label=True)



### Task 2: IRT: GRM ###

dat1 = read.csv(file.choose(), header=T)                ## Import data

dat1[,c(1,6)] <- 6-dat1[,c(1,6)]    ## reverse code items I1 & I13

dat1    ## check if reverse coded

## Examine uni-diemnsionality

cor_result <- cor(dat1, use="complete.obs")           ## confirm ploychoric correlation

eigen(cor_result)$value                               ## To check uni-dimensionality, check eigenvalues

VSS.scree(cor_result,main="scree plot")    ## depict the scree plot 

descript(dat1)     ## If there are items without any options, need to take care of them

## if you find items with an option that no one selected and get an error in R
## one solution as Huub Hoofs suggested is to subtract 1 from the variable(s) that have counts of 0

dat2 <- within(dat1, I8 <- (I8 - 1))     
dat3 <- within(dat2, I16 <- (I16 - 1))

descript(dat3)

## Estimation of IRT parameters with GRM 

para_grm <- grm(dat3, IRT.para=T)   ## estimate parameters
para_grm                            ## To see results, write the save data name ("para_grm" in this code)
coef(para_grm)                      ## When you want to see only parameter estimates
summary(para_grm)                   ## When you want to see all indecies

## Export the parameters

#when there is/are item(s) with an option that no one selected
result <- coef(para_grm)

result1<- rbind(result$I1, result$I2, result$I11, result$I12, result$I13, result$I14, result$I15, result$I17)
label_1<-c("item I1", "item I2", "item I11", "item I12", "item I13", "item I14", "item I15", "item I17")  
result2<-cbind(label_1, result1)

result3<- rbind(result$I8, result$I16)
label_2<-c("item I8", "item I16")  
emp_col <- rep("NON", 2)
result4<-cbind (label_2, emp_col, result3)

final<-rbind(result2, result4)

write.csv(final,"task2_result.csv")      ## export the result 

## Depict IRCCC

plot(para_grm, type = "ICC", legend = TRUE, items=1) # IRCCC for item 1 
plot(para_grm, type = "ICC", legend = TRUE, items=2) # IRCCC for item 2 
plot(para_grm, type = "ICC", legend = TRUE, items=3) # IRCCC for item 3 
plot(para_grm, type = "ICC", legend = TRUE, items=4) # IRCCC for item 4 
plot(para_grm, type = "ICC", legend = TRUE, items=5) # IRCCC for item 5 
plot(para_grm, type = "ICC", legend = TRUE, items=6) # IRCCC for item 6 
plot(para_grm, type = "ICC", legend = TRUE, items=7) # IRCCC for item 7 
plot(para_grm, type = "ICC", legend = TRUE, items=8) # IRCCC for item 8 
plot(para_grm, type = "ICC", legend = TRUE, items=9) # IRCCC for item 9 
plot(para_grm, type = "ICC", legend = TRUE, items=10) # IRCCC for item 10
