library(FactoMineR)

install.packages("FactoMineR",dep=T)

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
library(ggplot2)

colnames(data_in)



# data_ieq <- data_in[,c(22, 25:34)] Forget activity dude!!

data_in <- read.csv('C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data\\min_data\\HLM_mod.csv',header=T)


data_ieq <- data_in[,26:35]
data_out <- data_in$Room 

cormat <- round(cor(data_ieq),2)
## O2 and sound seem to be correlated above 0.87. We can ignore this.

data_in_scale <- scale(as.data.frame(data_ieq), center=TRUE, scale = TRUE)

data_scale <- data.frame(Room = data_out,data_in_scale)

data_XY <- data.frame(Y = data_out,X = data_in_scale)
data_scale_manova <- manova(data_in_scale ~ data_out)

summary.aov( data_scale_manova)
# Even though Activity comes as significant, we have to keep it outside (just a coincidence?)
# Just PM came insignificant

## Test individual fits, just in case

summary(aov(data_in_scale[,10] ~ data_out))

lda.model.normalized <- lda(Room ~ .,data_scale)

plot(lda.model.normalized)

lda.model.normalized.CV <- lda(Room ~ . , data_scale, CV = TRUE)

variables <- round(as.data.frame(lda.model.normalized[4]),2)
write.csv(variables,"C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data\\min_data\\LDA_coeff.csv")
variables$var <- row.names(variables)
variables[with(variables, order(-LD1)),]

ct <- table(data_in$Room, lda.model.normalized.CV$class)

# total percent correct
sum(diag(prop.table(ct)))

plot(lda.model, dimen=1, type="both")

colnames(data_in)
data_SDNN <- data_in[,6]
data_SDNN_scaled <- scale(data_SDNN,center=T,scale=T)

## Significants in HRV data: Heart Rate BPM, VLF, LF, HF, 
summary(aov(data_in[,17] ~ data_out)) 

data_phy <- data_in[,4:16]
data_hrv_var_scale <- scale(as.data.frame(data_phy), center=TRUE, scale = TRUE)
#data_hrv_scale <- data.frame(Y = data_out,X = data_hrv_var_scale)
data_hrv_scale_manova <- manova(data_hrv_var_scale ~ data_out)

summary.aov( data_hrv_scale_manova)

"
create.pca <- PCA(data_scale,quali.sup=1)
plot(create.pca, habillage=1 , 
     col.hab = c("green", "red"), 
     title = "Room differences")
"
data.m <- melt(data_scale)

ggplot(data.m, aes(x = variable, y = value, fill = Room)) +
  geom_boxplot() +
  scale_fill_manual(values = c("gold","darkgreen"))

#boxplot(data_in_scale)

boxplot(data_out~data_in_scale, data=data_scale, col=(c("gold","darkgreen")))

attributes(summary(subset(data_scale,Room=="Room with PLANTS")))
as.matrix(summary(subset(data_scale,Room=="Room with BOXES")))


#write.csv(summary(subset(data_scale,Room=="Room with PLANTS"))[4,],"summaryplants.csv")
#write.csv(summary(subset(data_scale,Room=="Room with BOXES"))[4,],"summaryroom.csv")

write.csv(summary(subset(data_in,Room=="Room with PLANTS"))[4,],"C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data\\min_data\\summaryplants.csv")
write.csv(summary(subset(data_in,Room=="Room with BOXES"))[4,],"C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data\\min_data\\summaryroom.csv")
