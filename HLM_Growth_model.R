install.packages("klaR",dep=T)

install.packages("RMySQL",dep=T)

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') 
library(rJava)
library(xlsx)
library(reshape)
library(xts)
library(memisc)
library(DataCombine)
library(RMySQL)
library(MASS)
library(klaR)
install_github('slidify', 'ramnathv', ref = 'dev')
library(bayesm)
library(multilevel)
library(lme4)

## Steps from the Bleise document ## for R and HLM
## Data is from 

data_hlm <- read.csv('C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Model\\R codes\\Data_collation\\R_HLM_model\\HLM_mod.csv',header=T)

data_hlm$Timestamp <- as.POSIXct(strptime(data_hlm$Timestamp, "%Y-%m-%d %H:%M:%S"))

data_hlm$Time <- as.POSIXct(strptime(data_hlm$Time, "%H:%M:%S"))

#Step by step...
## Aggregate ?? & rename to G.variables 
## ANOVA for ICC1 & ICC2

colnames(data_hlm)

data_hrv <- data_hlm[,4:16]
Activity <- data_hlm[,22]
Participant <- data_hlm[,23]
Timestamp <- data_hlm[,2]
Room <- data_hlm[,3]
data_ieq <- data_hlm[,25:34]
Gender <- data_hlm[,35]
Time <- data_hlm[,37]
ToD <-data_hlm[,36]

Time_seconds <- as.numeric(Time) - min(as.numeric(Time))

## Scale the quants
data_hrv_s <- scale(data_hrv[,1:9],center=T,scale=T)
data_ieq_s <- scale(data_ieq,center=T,scale=T)
Activity_s <- scale(Activity,center=T,scale=T)

colnames (data_hrv_s)
lda.model.normalized <- lda(Room ~ data_ieq_s)

data_scale <- data.frame(Room = Room,data_ieq_s)
data.m <- melt(data_scale)

ggplot(data.m, aes(x = variable, y = value, fill = Room)) +
  geom_boxplot() +
  scale_fill_manual(values = c("gold","darkgreen"))

### Similarly for the HRV guys

data_hrv_scale <- data.frame(Room = Room,data_hrv_s[,c(1:9,13)])
data.h <- melt(data_hrv_scale)

ggplot(data.h, aes(x = variable, y = value, fill = Room)) +
  geom_boxplot() + ylim(-1.5,1.5) +
  scale_fill_manual(values = c("brown","green"))

lda.hrv.normalized <- lda(Room ~ data_hrv_s[,c(1:9,13)])
lda.hrv.normalized

summary.aov(manova(cbind(data_hrv_s[,1:5],data_hrv_s[,7:9],data_hrv_s[,13],Activity_s)~Room))

summary.aov(manova(data_ieq_s~Room))

null.model1<-aov(data_hrv_s[,3]~Room)
ICC1(null.model1)
ICC2(null.model1)

#Intercept variability for participants:
  
null.model2<-aov(data_hrv_s[,3]~Participant)
ICC1(null.model2)
ICC2(null.model2)

colnames(data_hrv_s)[3] <- "SDNN"

cond_dataset <- data.frame(RR = data_hrv_s[,2], SDNN = data_hrv_s[,3],RMSSD = data_hrv_s[,4], HeartRate = data_hrv_s[,1],
                           pNN50=data_hrv_s[,5],LF=data_hrv_s[,8],LFHF = data_hrv_s[,13], HF = data_hrv_s[,9],Room,Participant,Activity,Timestamp,Time_seconds, data_ieq_s,ToD)

input_dataset <- data.frame(RR = data_hrv_s[,2],Room,Participant,Activity,Timestamp,Time_seconds, data_ieq_s,ToD)


# ICC(1) value indicates x% of the variance in Output can be “explained” by group level.
# High ICC(2) value indicates that groups can be 'reliably' differentiated in terms of Output.

#Model.3 <- lmer(HRV ~ Temp + Humidity + (Temp+Humidity|Space),data=data_in)

# Uncoditional means model

Null.Model1<-lme(RR~1,random=~1|Room,data=cond_dataset, control=list(opt="optim"))
Null.Model2<-lme(RR~1,random=~1|Participant,data=input_dataset, control=list(opt="optim"))

Null.Model1<-lme(RR~1,random=list(~1|Participant, ~1|Room),data=cond_dataset, control=list(opt="optim"))

#VarCorr(Null.Model1)

#Unconstrained (Null) model
#Random Intercepts model - Level 1 only
#Means as outcome model - Level 2 only

##Random intercepts & Slopes model - Add random effects or fixed effects (your choice)

## Histograms , pairwise scatters of all inputs and outputs!!!

colnames(cond_dataset)

model.1r <- lme(SDNN ~ -1 + CO +CO2+Humidity+Light+NO2+O2+PM+Sound+Temperature+TVOC ,random=~1|Room,data=cond_dataset,control=list(opt="optim"))
summary(model.1r)


model.1p <- lme(SDNN ~ -1 +CO +CO2+Humidity+Light+NO2+O2+PM+Sound+Temperature+TVOC ,random=~1|Participant,data=cond_dataset,control=list(opt="optim"))
summary(model.1p)

model.2p <- lme(RR ~ -1 +CO +CO2+Humidity+Light+O2+PM+Temperature+TVOC ,random=~1|Participant,data=cond_dataset,control=list(opt="optim"))
summary(model.2p)

model.none <- lm(SDNN ~ -1+ CO +CO2+Humidity+Light+NO2+O2+PM+Sound+Temperature+TVOC,data=cond_dataset)
summary(model.none)

model.none2 <- lm(LFHF ~ -1 + CO +CO2+Humidity+Light+NO2+O2+PM+Sound+Temperature+TVOC,data=cond_dataset)
summary(model.none2)

model.none3 <- lm(RMSSD ~  -1 + CO +CO2+Humidity+Light+NO2+O2+PM+Sound+Temperature+TVOC,data=cond_dataset)
summary(model.none3)

model.none4 <- lm(HF ~ -1 + CO +CO2+Humidity+Light+NO2+O2+PM+Sound+Temperature+TVOC,data=cond_dataset)
summary(model.none4)

model.none4 <- lm(RR ~ -1 + CO +CO2+Humidity+Light+NO2+O2+PM+Sound+Temperature+TVOC +ToD+ Activity +Time_seconds,data=cond_dataset)
summary(model.none4)

model.none5 <- lm(RR ~ -1 + Humidity+Light+Sound+Temperature+ToD,data=cond_dataset)
summary(model.none5)

model.6pr <- lme(RR ~ -1 + CO + Activity+Humidity+Light+Temperature+Time_seconds+I(Time_seconds^2),random=list(~(Sound+O2)|Room,~ Temperature|Time, ~1|Participant),data=cond_dataset)
summary(model.6pr)

## Just for fun:
model.upd<-update(model.6pr,random=list(~1|Room,~ 1|Time, ~1|Participant),data=cond_dataset)
summary(model.upd)
anova(model.upd,model.6pr)

## Both
model.1pr <- lme(SDNN ~ -1 +CO +CO2+Humidity+Light+NO2+O2+PM+Sound+Temperature+TVOC ,random=~1|Participant+1|Room,data=cond_dataset,control=list(opt="optim"))
summary(model.1pr)

predict(model.1p)
d<- table(predict(model.1p),cond_dataset$SDNN)

model.11 <- lme(RMSSD ~ CO +CO2+Humidity+Light+NO2+O2+PM+Sound+Temperature+TVOC ,random=~1|Room,data=cond_dataset,control=list(opt="optim"))
summary(model.11)

colnames(cond_dataset)

data.frame(Variable = mult.icc(cond_dataset[,1:8],cond_dataset$Room)[,1],
      round(mult.icc(cond_dataset[,1:8],cond_dataset$Room)[,2:3],2))
mult.icc(cond_dataset[,1:5],cond_dataset$Participant)

mult.icc(cond_dataset[,12:21],cond_dataset$Room)
mult.icc(cond_dataset[,12:21],cond_dataset$Participant)

mult.icc(cond_dataset[,1:5],cond_dataset$ToD)
mult.icc(cond_dataset[,12:21],cond_dataset$ToD)

summary(aov(Activity~ToD))


acf(cond_dataset$SDNN)

Participant <-as.factor(Participant)
plot(Participant,Activity)
plot(cond_dataset$Time_seconds)
plot(cond_dataset[,1:8])
plot(cond_dataset[,14:24])

#All of them seem to have auto-correlations, so growth model is best, FOR NOW!

#First do an xyplot in time to determine groups changing wrt time!

#Also use mult.icc (to determine, intergroup variability of groups of variables among participants)
