rm(list = ls(all = TRUE))
#load needed libraries
library(FactoMineR)
library(psych)
library(gdata)
library(ggplot2)
library(plotrix)
library(data.table)
library(tidyr)
library(reshape2)
library(ggfortify)
library(qdapTools)
library(lme4)
library(QuantPsyc)

##### change current group to select dataset
##### note:separate scripts for Experiments 3 and 4b
currentgroup = c("mturk")
currentcondition = c("1","2")

## prep; load data files & set parameters
masterdata  = read.table(file = "/Users/jennywang/Google Drive/Museum/mathitude/data/alternative analyses/mathitudemaster.csv", sep=",",header=TRUE, na.strings="")
data = subset(masterdata,masterdata$Group %in% currentgroup)
data = subset(data,data$Condition %in% currentcondition)
data_t = data
data_t[,24:37]=(as.numeric(data[,24:37]=="T"|data[,24:37]=="O"))

cols<- c(a.born="royalblue1",b.mature="skyblue2",c.teach="darkorange1",d.observe="orange")

read <- c("how.read")
sensory <- c("how.see","how.hear")
core <- c("how.help","how.number","how.objperm","how.gravity","how.face","how.depth","how.color")
cog <- c("how.help","how.number","how.objperm","how.gravity","how.face")
types <- c("read","sensory","core")
allwhen<-c("when.read","when.help","when.number","when.objperm","when.gravity","when.face","when.depth","when.color","when.hear","when.see")
empirical = c(6, 0.5, 0.0055,0.29, 0.25, 0.0019, 0.0054, 0.33, 0.0027, 0.052)

####################################
### overall descriptives based on timeline choices and proportion responses for plotting
des_m = describeBy(data,group=NULL,na.rm=TRUE)
dat_m = data[,24:33]
table_m= mtabulate(dat_m)
time_m = des_m[10:19,]
rownames(time_m) <- c("read","help","number","objperm","gravity","face","depth","color","hear","see")
how_m = table_m/rowSums(table_m,na.rm=FALSE)
colnames(how_m) <- c("b.mature","a.born","d.observe","c.teach")
summ_m = cbind(time_m,how_m)
summ_m$Category.num = c(10,9,8,7,6,5,4,3,2,1)
##age estimates based on actual findings
summ_m$emp.age = empirical
##converting timeline choice mean/se to age estimate
summ_m$mean.estimate = 0.13*exp(0.78*summ_m$mean)
summ_m$se.estimate = 0.13*exp(0.78*summ_m$se)
summ_m$confIV1.estimate = summ_m$mean.estimate-1.96*summ_m$se.estimate
summ_m$confIV2.estimate = summ_m$mean.estimate+1.96*summ_m$se.estimate
##averages based on timeline choices
summ_core=colMeans(summ_m[2:8,]) 
summ_sense=colMeans(summ_m[9:10,]) 
summ_3 = rbind(summ_m[1,],summ_core,summ_sense)
summ_3$Category.num=c(3,2,1)
summ_3$age.ratio=summ_3$mean.estimate/summ_3$emp.age
rownames(summ_3)<-c("read","core","sensory")

## descriptives of learning-based scores
## linear models (t-tests, ANOVAs, linear regressions)
data[,10:19]=0.13*exp(0.78*data[,10:19])
data_t[,10:19]=0.13*exp(0.78*data_t[,10:19])
av.emp = mean(c(0.5, 0.0055,0.29, 0.25, 0.0019, 0.0054, 0.33))
how.summ = data_t[,c(1:9,38,39,40,41)]
output = data.frame(files=NULL)
summ.output = data.frame(files=NULL)
for(i in c(1:nrow(data_t))){
  how.sub = how.summ[i,]
  sum.sub = as.data.frame(lapply(how.sub, rep, 3))
  how.sub = as.data.frame(lapply(how.sub, rep, 10))
  for (k in c(24:33)){
    how.sub$Item[k-23] <- colnames(data_t)[k]
    how.sub$How[k-23] <- data_t[i,k]
    how.sub$When[k-23] <- data_t[i,k-14]
  }
  for(j in c(1:3)){
    sum.sub$Type[j]<-types[j]
    sum.sub$How[j] <- mean(how.sub$How[how.sub$Item %in% eval(parse(text = types[j]))],na.rm=TRUE)
    sum.sub$When[j] <- mean(how.sub$When[how.sub$Item %in% eval(parse(text = types[j]))],na.rm=TRUE)
  }
  output = rbind(output,how.sub)
  summ.output = rbind(summ.output,sum.sub)
}
summ.output$w.bias = summ.output$When-av.emp #core-default
summ.output[summ.output$Type=="read",]$w.bias=summ.output[summ.output$Type=="read",]$When-6
summ.output[summ.output$Type=="sensory",]$w.bias=summ.output[summ.output$Type=="sensory",]$When-mean(0.0027, 0.052)
summ.output$Type = as.factor(as.character(summ.output$Type))
summ.output$Condition = as.factor(as.character(summ.output$Condition))
summ.output$Group = as.factor(as.character(summ.output$Group))
summ.output$SubID = as.factor(as.character(summ.output$SubID))

## learning-bases scores
descr.how <-describeBy(summ.output$How,list(summ.output$Type),na.rm=TRUE,mat=TRUE)
descr.how$confIV1 = descr.how$mean-1.96*descr.how$se
descr.how$confIV2 = descr.how$mean+1.96*descr.how$se

output.core = subset(summ.output,summ.output$Type=="core")
t.test(output.core$How,mu=0.5)
output.read = subset(summ.output,summ.output$Type=="read")
output.sensory= subset(summ.output,summ.output$Type=="sensory")

# only cognitive abilities
output.cog = subset(output,output$Item %in% cog)
summ.output.cog = aggregate(output.cog,list(output.cog$SubID),mean,na.rm=TRUE)
t.test(summ.output.cog$How,mu=0.5)

## t-tests comparing core knowledge abilities to anchor items
t.test(summ.output[summ.output$Type=="core",]$w.bias,summ.output[summ.output$Type=="sensory",]$w.bias, paired=TRUE)
t.test(summ.output[summ.output$Type=="core",]$w.bias,summ.output[summ.output$Type=="read",]$w.bias, paired=TRUE)

## ANOVA comparing across conditions (e.g., wording conditions)
summary(aov(When~Type*Condition+Error(SubID/Type),summ.output))
summary(aov(When~Condition,output.core))

## comparing across groups (e.g., mturk vs. children)
t.test(How~Group,output.core)

## linear regressions (adjust factors based on experiment)
fit.lm.w <- lm(When~Condition+Gender+Parent+Age+Education,data=output.core)
lm.beta(fit.lm.w)
summary(fit.lm.w)

fit.lm.h <- lm(How~Condition+Gender+Parent+Age+Education,data=output.core)
lm.beta(fit.lm.h)
summary(fit.lm.h)

## descriptives of differing groups/conditions. adjust factors based on lm.
summ.w <- describeBy(output.core$When,output.core$Gender,na.rm=TRUE,mat=TRUE)
summ.w$confIV1 = summ.w$mean-1.96*summ.w$se
summ.w$confIV2 = summ.w$mean+1.96*summ.w$se

summ.h <- describeBy(output.core$How,output.core$Gender,na.rm=TRUE,mat=TRUE)
summ.h$confIV1 = summ.h$mean-1.96*summ.h$se
summ.h$confIV2 = summ.h$mean+1.96*summ.h$se


