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
