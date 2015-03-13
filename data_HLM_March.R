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

# Input the IEQ data

setwd("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data\\min_data\\one_minute_IEQ_data\\jan28to30_1_minute_data")

ieq_part_1 <- list()

temp = list.files(pattern="*.csv")

for (i in 1:length(temp)) 
{
  all_content = readLines(temp[i])
  skip_second = all_content[-c(1,2)]
  ieq_part_1[[i]] <- read.csv(textConnection(skip_second), header = FALSE)
  
  #ieq_part_1[[i]] <- as.data.frame(read.csv(temp[i],header=T))
}


setwd("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data\\min_data\\one_minute_IEQ_data\\jan30tofeb18_1_minute_data")

ieq_part_2 <- list()

temp = list.files(pattern="*.csv")

for (i in 1:length(temp)) 
{
  all_content = readLines(temp[i])
  skip_second = all_content[-c(1,2)]
  ieq_part_2[[i]] <- read.csv(textConnection(skip_second), header = FALSE)
  
  #ieq_part_2[[i]] <- as.data.frame(read.csv(temp[i],header=T))
}


ieq_all <- list()
ieq_melted <- list()
ieq_time2 <- list()
            
for (i in 1:length(ieq_part_2)) 
{
  colnames(ieq_part_2[[i]]) <- c("Epoch","Room with BOXES","Room with PLANTS")
  colnames(ieq_part_1[[i]]) <- c("Epoch","Room with BOXES","Room with PLANTS")
  ieq_all[[i]] <- rbind(ieq_part_2[[i]],ieq_part_1[[i]])
  ieq_melted[[i]] <- melt(ieq_all[[i]], id=c("Epoch"))
  ieq_time2[[i]] <- na.omit(ieq_melted[[i]]) ## Remove na and milliseconds resolution
  ieq_time2[[i]]$Epoch <- ieq_time2[[i]]$Epoch/1000 
}

colnames(ieq_time2[[1]]) <- c("Epoch","Room","CO")
colnames(ieq_time2[[2]]) <- c("Epoch","Room","CO2")
colnames(ieq_time2[[3]]) <- c("Epoch","Room","Humidity")
colnames(ieq_time2[[4]]) <- c("Epoch","Room","Light")
colnames(ieq_time2[[5]]) <- c("Epoch","Room","NO2")
colnames(ieq_time2[[6]]) <- c("Epoch","Room","O2")
colnames(ieq_time2[[7]]) <- c("Epoch","Room","PM")
colnames(ieq_time2[[8]]) <- c("Epoch","Room","Sound")
colnames(ieq_time2[[9]]) <- c("Epoch","Room","Temperature")
colnames(ieq_time2[[10]]) <- c("Epoch","Room","TVOC")

ieq_time3 <- ieq_time2

system.time(
  for (i in 1:length(ieq_time3)) 
  {
    ieq_time3[[i]]$Timestamp <- as.POSIXct(as.numeric(ieq_time3[[i]]$Epoch), origin = "1970-01-01") 
    ieq_time3[[i]] <- ieq_time3[[i]][,!(colnames(ieq_time3[[i]]) %in% c("Epoch"))]
  }
)

ieq_var <- ieq_time3[[1]]

for (i in 2: length(ieq_time3))
{
ieq_var <- merge(ieq_var,ieq_time3[[i]],by = c("Timestamp","Room"),all = TRUE)
}

write.csv(ieq_var,'C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data\\min_data\\IEQ_1min.csv')

### Now prepare the new Phy data ### All variables, in an automated code whatever!!
## Also TimeExpand it to a minute only

# Input the phy data 

setwd("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data\\min_data\\one_min_Phy_data\\1_min_Participant")

phy_part_1 <- list()

temp = list.files(pattern="*.xls")

for (i in 1:length(temp)) 
{
  phy_part_1[[i]] <- read.xlsx(temp[i], 2)
  colnames(phy_part_1[[i]])[1] <- c("Timestamp")
  #phy_part_1[[i]]$Timestamp <- as.POSIXct(strptime(phy_part_1[[i]][1], "%m/%d/%y %H:%M:%S"))
  #Implicitly gets converted to timestamp format
}

phy_part_2 <- phy_part_1

phy_part_2[[1]]$P_ID <- "101"
phy_part_2[[2]]$P_ID <- "102"
phy_part_2[[3]]$P_ID <- "103"
phy_part_2[[4]]$P_ID <- "104"
phy_part_2[[5]]$P_ID <- "105"
phy_part_2[[6]]$P_ID <- "106"
phy_part_2[[7]]$P_ID <- "107"

phy_all <- list()

for (i in 1: length(phy_part_2))
{
  phy_all <- rbind(phy_all,phy_part_2[[i]])
}

## Correction for fe partipants time stored in EST:

phy_all_b <- phy_all

# phy_all <- phy_all_b

for(i in 1:nrow(phy_all))
{
  if(phy_all$P_ID[i]== "101" || phy_all$P_ID[i]== "102" || phy_all$P_ID[i]== "103" || phy_all$P_ID[i]== "104" )
  {
    #phy_all$Timestamp <- phy_all$Timestamp - 30*60*7/100
    phy_all$Timestamp <- phy_all$Timestamp - 132.6316 ## Magic number for converting digits to timestamp for this type of data (??!)
  }
}


write.csv(phy_all,'C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data\\min_data\\Phy_1min.csv')

### Currently manually coding the Space information based on Summary log provided. In actual, project, can implement it programmatically

#Coding logic is this:
Plant 3:05 Box
Box 3:05 Plant
Plant 3:03 Box
Box 3:03 Plant
Box 2:57 Plant
Plant 3:57 Box
Plant 10:40 Box

phy_5min <- read.xlsx("C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data\\min_data\\Phy_1min_manual_coding_room.xlsx",1)

phy_1 <- phy_5min

# TimeExpand to per minute

phy_1$P_ID<-as.factor(phy_1$P_ID)
phy_part <- split(phy_1,phy_1$P_ID)

#h <- TimeExpand(g,TimeVar='Timestamp')
#h[,!colnames(h)%in% c("Timestamp","Room","P_ID")] <-na.locf(h[,!colnames(h)%in% c("Timestamp","Room","P_ID")])
#h[,colnames(h)%in% c("Room","P_ID")] <-na.locf(h[,colnames(h)%in% c("Room","P_ID")])



to_sec <- function(data_in){
  data_o <- TimeExpand(data_in,TimeVar='Timestamp')
  h <- data_o
  h[,!colnames(h)%in% c("Timestamp","Room","P_ID")] <-na.locf(h[,!colnames(h)%in% c("Timestamp","Room","P_ID")])
  h[,colnames(h)%in% c("Room","P_ID")] <-na.locf(h[,colnames(h)%in% c("Room","P_ID")])
  h$P_ID<-as.factor(h$P_ID)
  h$Room<-as.factor(h$Room)
  data_out <- h 
  return(data_out)
}

### Use zoo objects to aggregate to per minute from per second (instead of custom aggregate_function)

to_min <- function(data_in){
  h <- data_in
  data_num <- zoo(h[,!colnames(h)%in% c("Room","P_ID","Timestamp")],h$Timestamp)
  data_chr <- zoo(h[,colnames(h)%in% c("Room","P_ID")],h$Timestamp)
  data_out_num <- aggregate(data_num,time(data_num) - as.numeric(time(data_num)) %% 60, mean)
  data_out_chr <- aggregate(data_chr, time(data_chr) - as.numeric(time(data_chr)) %% 60, function(x) names(which.max(table(x))))
  data_out <- cbind(as.data.frame(data_out_num),as.data.frame(data_out_chr))
  return(data_out)
}


phy_part_sec <- list()
phy_part_min <- list()

for(i in 1:length(phy_part))
{
  phy_part_sec[[i]] <- to_sec(phy_part[[i]])
  phy_part_min[[i]] <- to_min(phy_part_sec[[i]])
}

phy_1min <- phy_part_min[[1]]

for (i in 2: length(phy_part_min))
{
 phy_1min <- rbind(phy_1min,phy_part_min[[i]])
}

phy_min <- as.data.frame(cbind(rownames(phy_1min),phy_1min))

phy_data <- phy_min

colnames(phy_data)[1] <- c("Timestamp")
phy_data$Timestamp <- as.POSIXct(phy_data$Timestamp)
phy_data$Timestamp <- round(phy_data$Timestamp,"mins")

#ieq_data$Timestamp <- round(ieq_var$Timestamp,"mins")

write.csv(phy_data,'C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data\\min_data\\Phy_1min.csv')

## Excel distorts the time format of R POSIXct. Hence make a custom format yyyy-mm-dd h:mm:ss before retrieving files 

phy_in <- read.csv('C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data\\min_data\\Phy_1_min.csv',header=T)

ieq_var2 <- read.csv('C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data\\min_data\\IEQ_1min.csv',header=T)

#a <- as.POSIXct(strptime(phy_in$Timestamp, "%Y-%m-%d %H:%M:%S"))

phy_in$Timestamp <- as.POSIXct(strptime(phy_in$Timestamp, "%Y-%m-%d %H:%M:%S"))

ieq_var2$Timestamp <- as.POSIXct(strptime(ieq_var2$Timestamp, "%Y-%m-%d %H:%M:%S"))

data_in <- merge(phy_in,ieq_var2,by = c("Timestamp","Room"))

write.csv(data_in,'C:\\Users\\karthik\\Google Drive\\INSITE Shared Folder\\GSA IEQ\\Data\\min_data\\HLM_in.csv')

"
phy_SDNN <- read.csv('G:\\Insite_2\\GSA_IEQ\\HLM\\SDNN.csv',header=T)

a <- na.omit(phy_SDNN[2])
b <- na.omit(phy_SDNN[1])

shapiro.test(a)

# Push to database 

cannon <- dbConnect(MySQL(),
                    user = 'root',
                    password = 'insite123',
                    host = 'localhost',
                    dbname='IEQUA')

dbWriteTable(conn = cannon, name = 'IEQ_Phy_1min', value = as.data.frame(data_in))

dbDisconnect(cannon)

"
