##################################################################
#                         START UP VMs                           #
##################################################################
#IF SOME THINGS NOT LOADED OR INSTALLED USE ...104.EVERYTHING...R#
##################################################################

##############################################
#             QA.ds.glm.5s.180116             #
##############################################

#TURN OFF WARNINGS TO AVOID INCORRECT WARNINGS
#options(warn=-1) #(to turn off warnings if object of class list appears and confuses search for opals)

# HOW MANY VMs TO USE 1,2 or 3
use.VMs<-3

#SET DEFAULT WORKING DIRECTORY
setwd("O:\\Documents\\pb51\\DATASHIELD.PROJECT\\STARTUP.AND.ANALYSIS.SCRIPTS")

#SOURCE CURRENT SET OF DEFAULT CLIENT FUNCTIONS THAT HAVE NOT BEEN ADDED TO dsBase.client
source("O:\\Documents\\pb51\\DATASHIELD.PROJECT\\STARTUP.AND.ANALYSIS.SCRIPTS\\CURRENT.EXTRA.CLIENT.SOURCE.FILES.R")

#AT PRESENT DOESN'T WORK ON WINDOWS MACHINES (HELPER FUNCTIONS DONT TRANSFER)
#update.packages(repos='http://cran.obiba.org', ask=FALSE, type='source')
#library(dsBaseClient)
#library(dsGraphicsClient)
#library(dsModellingClient)
#library(dsStatsClient)


#PACKAGES: devtools
library(devtools)

#PACKAGES: opal, opaladmin
library(opal)
library(opaladmin)


#When the current release is different to the most updated packages the
#following still work 
devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsBaseClient-master")
devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsStatsClient-master")
devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsGraphicsClient-master")
devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsModellingClient-master")
devtools::load_all("O:/Documents/pb51/DataSHIELD.PROJECT/DEVELOP.GITHUB/dsBetaTestClient-master")

####################################################### 
############# LOGIN PROCEDURE EXPANDED DATA ###########
#######################################################
if(use.VMs==1)
{
logindata.VMs.em<-ds.createLogindata(110,table="SURVIVAL.EXPAND_WITH_MISSING1")
opals.em <- datashield.login(logins=logindata.VMs.em,assign=TRUE,symbol="EM")
}else
if(use.VMs==2)
{
logindata.VMs.em<-ds.createLogindata(110,110,table=c("SURVIVAL.EXPAND_WITH_MISSING1","SURVIVAL.EXPAND_WITH_MISSING2"))
logindata.VMs.em<-logindata.VMs.em[1:use.VMs,]
opals.em <- datashield.login(logins=logindata.VMs.em,assign=TRUE,symbol="EM")
}else
if(use.VMs==3)
{
logindata.VMs.em<-ds.createLogindata(110,110,110,table=c("SURVIVAL.EXPAND_WITH_MISSING1","SURVIVAL.EXPAND_WITH_MISSING2","SURVIVAL.EXPAND_WITH_MISSING3"))
logindata.VMs.em<-logindata.VMs.em[1:use.VMs,]
opals.em <- datashield.login(logins=logindata.VMs.em,assign=TRUE,symbol="EM")
}


ds.dim('EM',datasources=opals.em)
ds.colnames("EM",datasources=opals.em)

ds.make("rep(5,1)","DANGER.nfilter.tab")
ds.make("rep(3,1)","DANGER.nfilter.subset")
ds.make("rep(0.37,1)","DANGER.nfilter.glm")

ds.look("DANGERprintDS(DANGER.nfilter.tab)")
ds.look("DANGERprintDS(DANGER.nfilter.subset)")
ds.look("DANGERprintDS(DANGER.nfilter.glm)")


ds.listOpals()

ds.setDefaultOpals("opals.em")
findLoginObjects()
default.opals
ds.colnames("EM")
 

#################################################################
#     ANALYSE DATA ON VMs - CODE FROM QA.ds.glm.5s.170116       #
#################################################################

ds.ListClientsideFunctions()
ds.ListServersideFunctions()

ds.colnames('EM')
ds.ListServersideFunctions()
ds.ListClientsideFunctions()



ds.asNumeric("EM$id","ID")
ds.asNumeric("EM$study.id","SID")
ds.asNumeric("EM$time.id","TID")
ds.asNumeric("EM$survtime","SURV")
ds.asNumeric("EM$cens","EVENT")
ds.asNumeric("EM$age.60","AGE.60")
ds.asFactor("EM$female","SEXF")

ds.asFactor("TID","TID.f")
ds.log("SURV",newobj="LOG.SURV")


#replace.NA.1.vect<-rep(1,use.VMs)

#ds.replaceNA("SURV",replace.NA.1.vect,"TEMP")

ds.make("ID-ID","ZEROS1")
ds.table1D("ZEROS1")
ds.histogram("EM$bmi.26")

ds.make("ID-ID","ZEROS")
ds.make("ZEROS+1","ONES")

opal1<-opals.em[1]
opal2<-opals.em[2]
opal3<-opals.em[3]

ds.ass("ONES","S.1",opal1)
ds.ass("ZEROS","S.1",opal2)
ds.ass("ZEROS","S.1",opal3)


ds.ass("ZEROS","S.2",opal1)
ds.ass("ONES","S.2",opal2)
ds.ass("ZEROS","S.2",opal3)

rm(opal1)
rm(opal2)
rm(opal3)


ds.quantileMean("ONES","split",datasources=opals.em[1])
ds.quantileMean("ZEROS","split")
ds.quantileMean("S.1","split")
ds.quantileMean("S.2","split")






ds.ass("ONES*4","WEIGHTS4",opals.em)


msg.age.tid<-ds.meanSdGp("AGE.60","TID.f")
msg.age.tid

mod1a0<-ds.glm("EVENT~1+TID.f+SEXF+AGE.60",family="poisson",offset="LOG.SURV")
mod1a0

mod1a1<-ds.glm.5s("EVENT~1+TID.f+SEXF+AGE.60",family="poisson",offset="LOG.SURV")
mod1a1

mod1a2<-ds.glm.5s("EVENT~0+TID.f+SEXF+AGE.60",family="poisson",offset="LOG.SURV")
mod1a2

#mod1a1.s<-ds.glm.5s.sensitive("EVENT~1+TID.f+SEXF+AGE.60",family="poisson",offset="LOG.SURV")
#mod1a1.s


mod1b<-ds.glm.5s("AGE.60~0+TID.f",family="gaussian")
mod1b

msg.age.tid
mod1b



mod1c<-ds.glm.5s("EVENT~1+TID.f+SEXF+AGE.60",family="poisson",weights="WEIGHTS4")
mod1c

mod1d<-ds.glm.5s("EVENT~1+TID.f+SEXF+AGE.60",family="poisson",offset="LOG.SURV",weights="WEIGHTS4")
mod1d

mod1e<-ds.glm.5s("EVENT~1+TID.f+SEXF+EM$age.60",family="poisson",offset="LOG.SURV",weights="WEIGHTS4")
mod1e

mod1f<-ds.glm.5s("EVENT~1+TID.f+SEXF*AGE.60",family="poisson",offset="LOG.SURV",weights="WEIGHTS4")
mod1f

mod1g<-ds.glm.5s("EVENT~0+TID.f+SEXF*AGE.60",family="poisson",offset="LOG.SURV",weights="WEIGHTS4")
mod1g

mod1h<-ds.glm.5s("EVENT~TID.f+SEXF*AGE.60",family="poisson",offset="LOG.SURV",weights="WEIGHTS4")
mod1h

mod1i<-ds.glm.5s("SURV~TID.f+SEXF*AGE.60",family="gaussian",offset="LOG.SURV",weights="WEIGHTS4")
mod1i

#CONFIRMS CORRECT ANSWERS MATHEMATICALLY
mod1j<-ds.glm.5s("SURV~1",family="gaussian")
mod1j

mod1k<-ds.glm.5s("SURV~SEXF",family="gaussian")
mod1k

ds.asNumeric("SEXF","SEXF.n")
ds.table1D("SEXF.n")
ds.ass("1-SEXF.n","SEXM.n")
ds.asFactor("SEXM.n","SEXM")
ds.table1D("SEXM")

mod1l<-ds.glm.5s("SURV~SEXM",family="gaussian")
mod1l

mod1m<-ds.glm.5s("EVENT~1",family="poisson",offset="LOG.SURV")
mod1m


mod1n<-ds.glm.5s("EVENT~SEXM",family="poisson",offset="LOG.SURV")
mod1n

ds.ass("LOG.SURV+4","LOG.SURV.miss")
ds.ass("log(LOG.SURV.miss)","LOG.SURV.miss")
ds.ass("exp(LOG.SURV.miss)","LOG.SURV.miss")
ds.ass("LOG.SURV.miss-4","LOG.SURV.miss")
ds.histogram("LOG.SURV.miss")

ds.class("LOG.SURV.miss")

#CHECK WORKS IF OFFSET HAS ADDITIONAL MISSINGS COMPARED TO YX
mod1o<-ds.glm.5s("EVENT~0+TID.f+SEXF*AGE.60",family="poisson",offset="LOG.SURV.miss")
mod1o

#CHECK WORKS WITH STUDY-LEVEL EFFECT ADDED
mod1p<-ds.glm.5s("EVENT~S.1+TID.f+SEXF*AGE.60",family="poisson",offset="LOG.SURV",weights="WEIGHTS4")
mod1p

#CHECK WORKS WITH LOGISTIC REGRESSION
mod1q<-ds.glm.5s("EVENT~SEXF+AGE.60",family="binomial")
mod1q

mod1r<-ds.glm.5s("EVENT~SEXF+AGE.60",family="binomial",weights="WEIGHTS4")
mod1r

#Confirm that check for direct specification of an offset or weights in
#model formula is active and works properly. If a variable has the text
#"offset" or "weights" in its name, the warning will be shown but
#the model will fit OK. If an offset or weights are directly specified in
#the formula the warning will be shown and the model will fail
 
ds.ass("AGE.60","VARWITHOFFSETINNAME")

mod1s<-ds.glm.5s("EVENT~SEXF+VARWITHOFFSETINNAME",family="binomial",weights="WEIGHTS4")
mod1s


mod1t<-ds.glm.5s("EVENT~SEXF+AGE.60+offset(LOG.SURV)",family="binomial",weights="WEIGHTS4")
print("This model was meant to fail")

mod1u<- ds.glm.5s("EVENT~SEXF+AGE.60",family="binomial",datasources=opals.em)
mod1u


mod1v<- ds.glm.5s("EVENT~SEXF+AGE.60",family="binomial",datasources=opals.em[1])
mod1v

mod1w<- ds.glm.5s("EVENT~SEXF+AGE.60",family="binomial",datasources=opals.em[2])
mod1w




