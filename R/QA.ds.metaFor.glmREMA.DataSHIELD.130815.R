##################################################################
#                         START UP VMs                           #
##################################################################
#IF SOME THINGS NOT LOADED OR INSTALLED USE ...104.EVERYTHING...R#
##################################################################

#TURN OFF WARNINGS TO AVOID INCORRECT WARNINGS
#options(warn=-1) #(to turn off warnings if object of class list appears and confuses search for opals)

# HOW MANY VMs TO USE 1,2 or 3
use.VMs<-3

#SET DEFAULT WORKING DIRECTORY
setwd("O:\\Documents\\pb51\\DATASHIELD.PROJECT\\STARTUP.AND.ANALYSIS.SCRIPTS")

#SOURCE CURRENT SET OF DEFAULT CLIENT FUNCTIONS THAT HAVE NOT BEEN ADDED TO dsBase.client
source("O:\\Documents\\pb51\\DATASHIELD.PROJECT\\STARTUP.AND.ANALYSIS.SCRIPTS\\CURRENT.EXTRA.CLIENT.SOURCE.FILES.R")


#PACKAGES: devtools
library(devtools)

#PACKAGES: opal, opaladmin
library(opal)
library(opaladmin)


#When the current release is different to the most updated packages the
#following still work 
devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsBaseClient")
devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsStatsClient")
devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsGraphicsClient")
#devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsModellingClient")
#devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsModellingClient-PB_ds.glm_bugfix")
devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsModellingClient-master")



#otherwise when package release is up to date can use
#install.packages('dsBaseClient', repos='http://cran.obiba.org', type='source')
#install.packages('dsModellingClient', repos='http://cran.obiba.org', type='source')
#install.packages('dsGraphicsClient', repos='http://cran.obiba.org', type='source')
#install.packages('dsStatsClient', repos='http://cran.obiba.org', type='source')

#Do the same for server side functions
#LOAD SERVER SIDE PACKAGES INTO R
#devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsBase/dsBase-master.110215")
#devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsGraphics/dsGraphics-master.110215")
#devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsModelling/dsModelling-master.110215")
#devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsStats/dsStats-master.110215")


#LOGIN PROCEDURE
if(use.VMs==1)
{
logindata.VMs.em<-ds.createLogindata(102,table="SURVSIM.EMISS")
opals.em <- datashield.login(logins=logindata.VMs.em,assign=TRUE,symbol="EM")
}else
if(use.VMs==2)
{
logindata.VMs.em<-ds.createLogindata(102,103,table="SURVSIM.EMISS")
opals.em <- datashield.login(logins=logindata.VMs.em,assign=TRUE,symbol="EM")
}else
if(use.VMs==3)
{
logindata.VMs.em<-ds.createLogindata(102,103,104,table="SURVSIM.EMISS")
opals.em <- datashield.login(logins=logindata.VMs.em,assign=TRUE,symbol="EM")
}



ds.dim('EM')

#################################################################
#                       ANALYSE DATA ON VMs                     #
#################################################################


ds.colnames('EM')

ds.ListServersideFunctions()



ds.asNumeric("EM$id","ID")
ds.asNumeric("EM$study.id","SID")
ds.asNumeric("EM$time.id","TID")
ds.asNumeric("EM$survtime","SURV")
ds.asNumeric("EM$cens","EVENT")
ds.asNumeric("EM$age.60","AGE.60")
ds.asFactor("EM$female","SEXF")

ds.asFactor("TID","TID.f")
ds.log("SURV",newobj="LOG.SURV")


ds.replaceNA("SURV",c(1,1,1),"TEMP")
ds.make("TEMP-TEMP","ZEROS")
ds.make("ZEROS+1","ONES")

ds.make("ONES","S.1",opals.em[1])
ds.make("ZEROS","S.1",opals.em[2])

ds.make("ONES*4","WEIGHTS4",opals.em)

mod1i<-ds.glm.5s("SURV~TID.f+SEXF*AGE.60",family="gaussian")
mod1i

glm1<-ds.glm.5s("EVENT~1+TID.f+SEXF+AGE.60",family="poisson",offset="LOG.SURV")
glm1

glm.REMA1<-ds.glm.REMA("EVENT~1+TID.f+SEXF+AGE.60",family="poisson",offset="LOG.SURV")


betas<-cbind(glm.REMA1$study.specific.output[[1]]$model.parameters[7,1],
      glm.REMA1$study.specific.output[[2]]$model.parameters[7,1],
      glm.REMA1$study.specific.output[[3]]$model.parameters[7,1])

ses<-cbind(glm.REMA1$study.specific.output[[1]]$model.parameters[7,2],
      glm.REMA1$study.specific.output[[2]]$model.parameters[7,2],
      glm.REMA1$study.specific.output[[3]]$model.parameters[7,2])

betas
ses

glm.REMA1$study.specific.output$study1$model.parameters
glm.REMA1$study.specific.output$study2$model.parameters
glm.REMA1$study.specific.output$study3$model.parameters


rma1<-ds.metaFor(betas,ses)
rma1

#test will pick up heterogeneity
rma1<-ds.metaFor(betas,ses/10)
rma1

