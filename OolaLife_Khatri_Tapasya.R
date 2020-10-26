rm(list=ls())

h <- read.csv("/ESS_Happiness.csv")

######### A. Preprocessing Data ##########

##### 1. Validate data - Remove missing rows#####
v=list() 
v[[1]] <- c(3:13,17,19:21,23,48,51,53,54,56) #col numbers w valid values <77
v[[2]] <- c(14:16,18,22,26:33,45:47,55) #col numbers w valid values <7
v[[3]] <- c(24:25,50) #col numbers w valid values <777
v[[4]] <- c(34:45) #col numbers w valid values = non blank

for (a in 1:length(v)){
  
  if (a==1){
    for(i in v[[1]]){
      indx <- which(h[,i]>=77)
      h <- h[-indx,]
    }
  }
  
  if (a==2){
    for (i in v[[2]]){
      indx <- which(h[,i]>=7)
      h <- h[-indx,]
    }
  }
  if (a==3){
    for(i in v[[3]]){
      indx  <- which(h[,i]>=777)
      h <- h[-indx,]
    }
  }
  if (a==4){
    for(i in v[[4]]){
      h <- na.omit(h) 
    }
  }
}

##### 2. Transform Features #####

summary(h)

###gndr###
h$female <- cut(h$gndr, breaks = c(-1,1,2), labels = c(0,1 ))
#head(h[,c("gndr","female")],10)
h <- subset(h, select=-c(gndr))

###Weights###
h$weights=h$dweight*h$pweight
h <- subset(h,select = -c(dweight,pweight))

###Faith indicators###
faith <- h[,c("stflife","ppltrst", "pplfair", "pplhlp","trstprl","trstlgl","trstplc","stfedu","stfhlth","rlgdgr","weights","female")]
str(faith)

###likert boxplots of faith indicators wrt stflife###
require(reshape)
require(ggplot2)
faith.melt <- melt(faith, id.vars = c("weights","female"))
head(faith.melt)
tail(faith.melt)
(faithplot1 <- ggplot(faith.melt,aes(variable,value,fill=female), weights=faith.melt$weights)+
  geom_boxplot()+
  coord_flip()+
  scale_y_continuous(">--- Low to High --->",limits = c(0,10), breaks = seq(0,10,1))+
  scale_fill_manual(values = c("blue","magenta"), name= "Gender",labels=c("Male","Female"))+
  xlab("FAITH indicators")+
  theme_minimal())


###fitness indicators -condensing into meaningful categories###
h$tvrange <- cut(h$tvtot, breaks=c(-1,3,6,7), labels=c("0-1.5hrs","1.5-3hrs","3+hrs"))
h$tvtot <- h$tvrange
h <- subset(h,select = -c(tvrange))

h$hlthhmp <- cut(h$hlthhmp, breaks = c(-1,1,2,3), labels = c("Yesalot","Yes-somewht","no"))

h$eatfruit <- cut(h$etfruit, breaks = c(-1,2,4,6,7), labels = c("2+day","4-7wk","<1-3wk","Nvr"))
h$etfruit <- h$eatfruit
h <- subset(h,select = -c(eatfruit))

h$etveg <- cut(h$eatveg, breaks = c(-1,3,5,6,7), labels = c("1mny_day","2.1-3X_wk","3.<1_wk","0"))
h$eatveg <- h$etveg
h <- subset(h,select = -c(etveg))

h$smoke = cut(h$cgtsmke, breaks = c(-1,1,4,5), labels = c("1Daily", "2LessOftn_past", "3Never"))
h$cgtsmke <- h$smoke
h <- subset(h,select = -c(smoke))

h$alcohol = cut(h$alcfreq, breaks = c(-1,1,4,6,7), labels = c("1daily", "2few.mth-manywk","3.<1_mth","4Never"))
h$alcfreq <- h$alcohol
h <- subset(h,select = -c(alcohol))

h$dosport = cut(h$dosprt, breaks = c(-1,0,1,6,7), labels = c("0", "1","3-6","7"))
h$dosprt <- h$dosport
h <- subset(h,select = -c(dosport))

h$cancer<- cut(h$hltprca, breaks = c(-1,1,2,3), labels = c("Yes","Past","Nvr"))
h <- subset(h,select = -c(hltprca))
dim(h)
###BMI - calculated###
h$bmi=round(h$weight/((h$height/100)^2))
h <- h[h$bmi!=142,] #excluding outlier
h <- subset(h,select=-c(height,weight))
dim(h)
###Bodytype - from BMI###
h$bodytype <- cut(h$bmi,breaks = c(-1,18.5,25,30,100), labels = c("1Underwt", "2NormalWt","3Overwt","4Obese"))
h <- subset(h,select = -c(bmi))
dim(h)
###grouping columns into one###
hlth<- h[,c("hltprhc","hltprhb","hltprbp","hltpral","hltprbn","hltprsc","hltprpa","hltprpf","hltprsd","hltprsh","hltprdi")]

require(plyr)
h<- mutate(h,chronic=ifelse(rowSums(hlth)==0,0,1))
h<- subset(h, select=-c(hltprhc,hltprhb,hltprbp,hltpral,hltprbn,hltprsc,hltprpa,hltprpf,hltprsd,hltprsh,hltprdi))

###agea###
h$age_group <- cut(h$agea, breaks = c(13,25,40,60,80,120),labels = c("14-25","25-40","40-60","60-80","80+"))
h$agea <- h$age_group
h <- subset(h,select = -c(age_group))
str(h)


#Family and Friends and Fun

###sclmeet###
h$sclfreq <- cut(h$sclmeet, breaks = c(-1,1,2,3,4,5,7), labels = c("Nevr","<1mth","1_mth","many_mth","1_wk",">1wk-daily"))
h$sclmeet <- h$sclfreq
h <- subset(h,select = -c(sclfreq))


### inprdsc###
h$closfrnds <- cut(h$inprdsc, breaks = c(-1,0,1,2,6), labels = c("0","1","2","3+"))
h$inprdsc <- h$closfrnds
h <- subset(h,select = -c(closfrnds))

### sclact###
h$socialzg <- cut(h$sclact, breaks = c(-1,1,2,5), labels = c("very<most","<most","same->most"))
h$sclact<-h$socialzg
h <- subset(h,select = -c(socialzg))

###cnfpplh####
h$conflict <- cut(h$cnfpplh, breaks = c(-1,2,3,5),labels=c("1Alwys_ofn","2Smtims","3No-Rarely"))
h$cnfpplh <- h$conflict
h <- subset(h,select = -c(conflict))

###fnsdfml###
h$finan_diff <- cut(h$fnsdfml, breaks = c(-1,2,3,5),labels=c("1Alwys_ofn","2Smtims","3No-Rarely"))
h$fnsdfml <- h$finan_diff
h <- subset(h,select = -c(finan_diff))

###hhmmb###
h$numpplhouse <- cut(h$hhmmb, breaks = c(-1,1,3,6,9,13),labels = c("1","3","4-6","7-9","10-13"))
h <- subset(h,select = -c(hhmmb))

###maritalb###
h$maritalst <- cut(h$maritalb, breaks = c(-1,2,4,5,6), labels = c("1Married_cvlu","2sep_divor","3widowd","4Notmarried"))
h$maritalb <- h$maritalst
h <- subset(h,select = -c(maritalst))

###chldhm###
h$chldhome <- cut(h$chldhm, breaks =c(-1,1,2), labels = c(1,0))
h$chldhm <- h$chldhome
h <- subset(h,select = -c(chldhome))

###Field###
###eisced###
h$edu<-cut(h$eisced, breaks = c(-1,2,4,5,7,55),labels = c("<=lowerSec","2upperSec","3advVoc", "4BAandhigher","5other"))
h$eisced <- h$edu
h <- subset(h,select = -c(edu))

str(h)
###mnactic###
h$mnactic<-cut(h$mnactic, breaks = c(-1,1,2,4,5,6,7,8,9),labels = c("1Paidwork","2education","3umemployed", "4Sick_disabled","5retired","6service","7housework_childcare","8other"))

#x<-predict(dummyVars(~a, data=h), newdata=h)
#h<-cbind(h,x)
#h <- subset(h,select = -c(a))

###Finance###
###hincfel###
h$incomefeel<-cut(h$hincfel, breaks = c(-1,1,2,3,4),labels = c("1comfortable","2coping","3difficult", "4v.difficult"))
h$hincfel <- h$incomefeel
h <- subset(h,select = -c(incomefeel))

str(h)

###Boxplots###
par(mfrow=c(1,1))
all.box=function(x) {
  for (i in seq_along(x[c(-1,-2,-42,-40)])) {
    b<-boxplot(x$stflife~as.numeric(x[,i]),axes=TRUE,main = names(x)[i])
    Axis(side=1, labels=TRUE)
    Axis(side=2, labels=TRUE)
  }
  return(b)
}
all.box(h)
dev.off()

###stflife###
h$stflife <-cut(h$stflife, breaks = c(-1,5,10),labels = c("Dissatisfied","Satisfied"))
str(h)

########## B. Divide data into Training and Test##########

AT = which(h$cntry=="AT")
BE = which(h$cntry=="BE")
CH = which(h$cntry=="CH")
CZ = which(h$cntry=="CZ")
DE = which(h$cntry=="DE")
DK = which(h$cntry=="DK")
EE = which(h$cntry=="EE")
ES = which(h$cntry=="ES")
FI = which(h$cntry=="FI")
FR = which(h$cntry=="FR")
GB = which(h$cntry=="GB")
HU = which(h$cntry=="HU")
IE = which(h$cntry=="IE")
IL = which(h$cntry=="IL")
LT = which(h$cntry=="LT")
NL = which(h$cntry=="NL")
NO = which(h$cntry=="NO")
PL = which(h$cntry=="PL")
PT = which(h$cntry=="PT")
SE = which(h$cntry=="SE")
SI = which(h$cntry=="SI")
set.seed(51)
outs = c(sample(AT,round(length(AT)/3)),
         sample(BE,round(length(BE)/3)),
         sample(CH,round(length(CH)/3)),
         sample(CZ,round(length(CZ)/3)),
         sample(DE,round(length(DE)/3)),
         sample(DK,round(length(DK)/3)),
         sample(EE,round(length(EE)/3)),
         sample(ES,round(length(ES)/3)),
         sample(FI,round(length(FI)/3)),
         sample(FR,round(length(FR)/3)),
         sample(GB,round(length(GB)/3)),
         sample(HU,round(length(HU)/3)),
         sample(IE,round(length(IE)/3)),
         sample(IL,round(length(IL)/3)),
         sample(LT,round(length(LT)/3)),
         sample(NL,round(length(NL)/3)),
         sample(NO,round(length(NO)/3)),
         sample(PL,round(length(PL)/3)),
         sample(PT,round(length(PT)/3)),
         sample(SE,round(length(SE)/3)),
         sample(SI,round(length(SI)/3))
)
h.train = h[-outs,-which(names(h) %in% c("cntry","idno"))]
h.test = h[outs,-which(names(h) %in% c("idno", "cntry"))]

str(h.train)

########## C. Explore Data (for classification)##########

#require(psych)
#par(mfrow=c(1,1))
s <- which(names(h.train) %in% c("stflife"))
w <- which(names(h.train) %in% c("weights"))
#pairs.panels(h.train[,c(s,1:11)],ellipses=FALSE, scale=TRUE, gap=0.1)
#pairs.panels(h.train[,c(s,12:22)],ellipses=FALSE,scale=TRUE, gap=0.1)
#pairs.panels(h.train[,c(s,23:34)],ellipses=FALSE, scale=TRUE, gap=0.1)
#pairs.panels(h.train[,c(s,35:37,39,41:44)],ellipses=FALSE,scale=TRUE, gap=0.1)


########### D. Analysis using k- nearest neighbour classifier ###########
require(kknn)
source("/classificationMetrics.R")
kseq=seq(1,51,2)
myref <- h.train$stflife
kernel <- c("optimal","triangular")
cat(date(),'\n')
set.seed(501)
kkcv<-train.kknn(stflife~., data=h.train[-w], weights= h.train$weights,ks=kseq, kernel=kernel,distance=1) 
cat(date(),'\n')
summary(kkcv)

ktune_stats <- data.frame(k=sapply(kkcv$fitted.values,function(x) attr(x,'k')), kernel=sapply(kkcv$fitted.values,function(x) attr(x,'kernel')))
ktune_stats$inverr<-sapply(kkcv$fitted.values,weightedScore,ref=myref)
ktune_stats$flaterr<-sapply(kkcv$fitted.values,flatErr,ref=myref)

#Winning setting
ktune_stats[which.min(ktune_stats$inverr),]

##confusion matrix for winning setting of KKNN cv on training data:
kkcv.bestinv=kkcv$fitted.values[[which.min(ktune_stats$inverr)]]
cm.inverr<-table(myref,kkcv.bestinv)
classErr <- rbind(round(cm.inverr[3]/(cm.inverr[3]+cm.inverr[1]),2),round(cm.inverr[2]/(cm.inverr[2]+cm.inverr[4]),2))
(cm.inverr <-cbind(cm.inverr,classErr))

plot(kseq, kkcv$MISCLASS[,1], ylab= "Error Rates",type="l", 
     ylim = c(0.15, 0.38), main="KNN tuning for Flat and Inverse error rates")
lines(kseq, ktune_stats$inverr[1:26], col=2)
legend("bottomright", lty=1,col=1:2,legend = c("Flat","Inverse"))

#kconf <- attr(train.kknn(stflife~.,data=h.train[-w], weights=h.train$weights,ks=9,kernel="optimal", distance=1))
#table(round(kconf,2))
ppv<-function(x,ref,targlev=2)
{ 
  if(!any(ref==targlev)) stop("Target level not present in reference vector.\n")
  ppv=sum(x %in% targlev & ref %in% targlev)/sum(x %in% targlev)
  return(ppv)
}
(senSpec(kkcv.bestinv, myref, targlev="Satisfied"))
(ppv(kkcv.bestinv, myref, targlev="Satisfied"))

#predicting on the test data
cat(date(),'\n') 
w2 <- which(names(h.test) %in% c("weights"))
set.seed(501)
kkpred <- kknn(stflife~., train=h.train[-w],test=h.test[-w2],kernel = "optimal", k=9)

(weightedScore(kkpred$fitted.values,ref = h.test$stflife))
(confkk<-table(ref=h.test$stflife, kkpred$fitted.values))
cat(date(),'\n') 
classerr=rbind(round(confkk[3]/(confkk[1]+confkk[3]),2), round(confkk[2]/(confkk[4]+confkk[2]),2))
(confmatkk<-cbind(confkk,"Classerr"=classerr))
(senSpec(kkpred$fitted.values, h.test$stflife, targlev="Satisfied"))
(ppv(kkpred$fitted.values, h.test$stflife, targlev="Satisfied"))

########### E. Analysis using Random Forest classifier ###########
source("/classificationMetrics.R")
require(randomForest)
require(doSNOW)
cl <- makeCluster(2, type= "SOCK")
registerDoSNOW(cl)
bseq <- 100*c(1:5)
n <- nrow(h.train)
k <-length(bseq)
htune=matrix(NA,nrow=n,ncol=k) 
errorate=function(x,ref) mean(x!=ref) 
cat(date(),'\n')
for(a in 1:k){
  set.seed(501)
  htune[,a] <- randomForest(stflife~., data=h.train[-w],weights=h.train$weights, ntree = bseq[a])$predicted 
}

rbind(bseq,round(100*apply(htune,2,errorate, ref = htune[,k]),1))
cat(date(),'\n')
stopCluster(cl)

###3D tuning using penalty, #features, nodesize)
require(doParallel)
myref <- as.numeric(h.train$stflife)
pvec<-table(h.train$stflife)/nrow(h.train)
ftune1<-expand.grid(m=c(5,10,15,20),detail=c(3,5,7,9))
ncore<-2
cl <- makeCluster(ncore, type = "SOCK") 
registerDoParallel(cl) 
cat(date(),'\n')  
morerrs<-foreach(a=1:dim(ftune1)[1],.combine='rbind',.packages='randomForest') %dopar% {    
  set.seed(501)     
  tmp=randomForest(stflife~.,data=h.train[-w],ntree=500, weights = h.train$weights,mtry=ftune1$m[a],        
                   nodesize=ftune1$detail[a],cutoff=pvec,strata="stflife")   
  data.frame (flat=flatErr(tmp$predicted,h.train$stflife),        
              inv=weightedScore(as.numeric(tmp$predicted),myref))  
} 
cat(date(),'\n')
stopCluster(cl)
(ftune1=cbind(ftune1,morerrs))

ftune1[which.min(ftune1$inv),]

library(latticeExtra) 
detach(ggplot2,unload = T)
set.seed(501)
levelplot(inv~m+detail,data=ftune1,panel=panel.levelplot.points,cex=3,pch=22,    
          col.regions=rainbow(31,start=2/3),main="Random-Forest Tuning Map: Inverse Loss") +
  layer(panel.2dsmoother(...,col.regions=rainbow(31,start=2/3,alpha=0.7)))

set.seed(501) ## thats the seed under which it was a winner 
hforest1=randomForest(stflife~.,data=h.train[-w],ntree=500, weights = h.train$weights,mtry=5,        
                      nodesize=9,cutoff=pvec,strata="stflife")
rf_confusn<-table(h.train$stflife,hforest1$predicted)
(cbind(rf_confusn,"classerr"=rbind(rf_confusn[3]/(rf_confusn[1]+rf_confusn[3]), rf_confusn[2]/(rf_confusn[4]+ rf_confusn[2]))))
par(mfrow=c(1,1))
boxplot(apply(hforest1$votes,1,max)~(h.train$stflife==hforest1$predicted),main="RF-confidence intervals:missed observations",varwidth=TRUE,col=3,range=0.5)

varImpPlot(hforest1,pch=19,col=4,main = "Random forest Variable importance plot")

###Predicting the test
myref2 <- h.test$stflife
best_rf <- hforest1
rf_pred <- predict(best_rf, newdata = h.test[-w2])
(rf_predconfusn=table(myref2,rf_pred))
(rf_predclass.er=cbind(rf_predconfusn,"classerr"=rbind(rf_predconfusn[3]/(rf_predconfusn[1]+rf_predconfusn[3]), rf_predconfusn[2]/(rf_predconfusn[4]+rf_predconfusn[2]))))








