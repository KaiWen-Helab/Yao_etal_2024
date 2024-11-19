#  The data temporally aligned to behavior initiation was pre-processed 
#  by software (Inper Data Processing-v07.2) and then exported in advance
#   The main folder conctain the data from PBS group or PFF group mice 
#     Each sub folder stores two (M1 and STR data) csv files with formatted naming ("trailsM1"or"trailsSTR") from one mouse
#By XRYao


library(ggplot2)
path<-"PATH_of_data_PBSgroup"  
path1<-"PATH_of_data_PFFgroup" 


#------Calculate the cross-correlaton between M1 and STR signal from PBS group ---------

dir.name <- dir(path) 
setwd(path)



for (i in 1:length(dir.name)) {
  #traverse all files(mice)
  
  fileM1_name <-  paste(path,dir.name[i],"trailsM1.csv",sep='/')
  fileSTR_name <-  paste(path,dir.name[i],"trailsSTR.csv",sep='/')
  data_1<- read.csv(fileM1_name,header = T)
  data_2<- read.csv(fileSTR_name,header = T)
  data_M1 <- data_1[1:min(nrow(data_1),nrow(data_2)),]; 
  data_STR <- data_2[1:min(nrow(data_1),nrow(data_2)),]; 
  averaged_M1<-rowMeans(data_M1[,-ncol(data_M1)])
  averaged_STR<-rowMeans(data_STR[,-ncol(data_STR)])
  #Every M1 or STR trials from the same mice was averaged firstly 
  
  ccf_result <- ccf(averaged_M1, averaged_STR, lag.max=80, plot = F) 
  
  xcorr_coef<- t(ccf_result$acf[,1,1]);
  write.table(xcorr_coef,file="xcorr.CSV", append = TRUE, sep = ",",row.names = F, col.names = F )
  #Save the resuls of cross-correlation analysis
}


Lag_seq <- ccf_result$lag[,1,1]/40
#Save the lag sequence

data_xcorr <- read.csv("xcorr.CSV",header = F)
Mean_xcorr<- as.numeric(colMeans(data_xcorr))

SEM <- apply(data_xcorr, 2, function(x) sd(x) / sqrt(length(x)))
#Calcualte the Standard Error of the Mean (SEM)

data_PBS <- data.frame(Lag_seq,Mean_xcorr,SEM)
#Integrate the analyzed result into a new dataframe


#------Do same for PFF group --------------------------------

dir.name <- dir(path1)
setwd(path1)
for (i in 1:length(dir.name)) {
  
  fileM1_name <-  paste(path1,dir.name[i],"trailsM1.csv",sep='/')
  fileSTR_name <-  paste(path1,dir.name[i],"trailsSTR.csv",sep='/')
  data_1<- read.csv(fileM1_name,header = T)
  data_2<- read.csv(fileSTR_name,header = T)
  data_M1 <- data_1[1:min(nrow(data_1),nrow(data_2)),]; 
  data_STR <- data_2[1:min(nrow(data_1),nrow(data_2)),]; 
  averaged_M1<-rowMeans(data_M1[,-ncol(data_M1)])
  averaged_STR<-rowMeans(data_STR[,-ncol(data_STR)])
  
  ccf_result <- ccf(averaged_M1, averaged_STR, lag.max=80, plot = F) 
  xcorr_coef<- t(ccf_result$acf[,1,1]);
  write.table(xcorr_coef,file="xcorr1.CSV", append = TRUE, sep = ",",row.names = F, col.names = F )
}

Lag_seq1 <- ccf_result$lag[,1,1]/40
data_xcorr1 <- read.csv("xcorr1.CSV",header = F)
Mean_xcorr1<- as.numeric(colMeans(data_xcorr1))
SEM1 <- apply(data_xcorr1, 2, function(x) sd(x) / sqrt(length(x)))
data_PFF <- data.frame(Lag_seq1,Mean_xcorr1,SEM1)


#------Input the dataframeS into ggplot function and ploting---------------------------

p1<-ggplot()+ 
  geom_ribbon(data = data_PFF, aes(x = Lag_seq1,y =Mean_xcorr1, ymin = Mean_xcorr1- SEM1, ymax = Mean_xcorr1+ SEM1) , 
              fill = "#ff0033",alpha = 0.3 )+ 
  geom_line(data = data_PFF, aes(x = Lag_seq1,y = Mean_xcorr1),color='#ff0033',size=1) +
  geom_ribbon(data = data_PBS, aes(x = Lag_seq,y =Mean_xcorr, ymin = Mean_xcorr- SEM, ymax = Mean_xcorr+ SEM) , 
              fill = "#330000",alpha = 0.3 )+ 
  geom_line(data = data_PBS, aes(x = Lag_seq,y = Mean_xcorr),color='#330000',size=1) + 
  geom_point( aes(x = Lag_seq[which.max(Mean_xcorr)], y = Mean_xcorr[which.max(Mean_xcorr)]), color = "blue", shape = 8, size = 0.7) +
  geom_point( aes(x = Lag_seq1[which.max(Mean_xcorr1)], y = Mean_xcorr1[which.max(Mean_xcorr1)]), color = "blue", shape = 8, size = 0.7) +
  theme_classic()+ 
  theme( axis.text = element_blank(),
         axis.line =element_line(size=0.5),
         axis.ticks =element_line(size=0.5) )+
  xlab(NULL)+ylab(NULL)+ 
  ylim(-0.5, 1) 

print(p1)

svg(file = "xcorr.svg",   
    width =1.5, 
    height =1.5 ,  
)

print(p1)
dev.off()


