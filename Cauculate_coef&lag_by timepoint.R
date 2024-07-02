# For analyzing the coupling index (Pearson's correlation coefficient) and phase lag between M1 and STR Ca2+ activity
#  The data temporally aligned to behavior initiation was pre-processed 
#  by software (Inper Data Processing-v07.2) and then exported in advance
#   Each sub folder stores two (M1 and STR data) csv files with formatted naming ("trailsM1"or"trailsSTR") from one mouse
#    Every M1 or STR trials from the same mice was averaged firstly, coupling index and phase lag was then calculated 
#     The result was written in a csv file by row, with the mouse number as row name
# By XRYao

path<-"Folder_PATH"
dir.name <- dir(path) 
setwd(path)
col_name <- data.frame('miceNo.','corr coef','lag')
write.table(col_name, file = "result.CSV", append = TRUE,sep = ",",row.names = F, col.names = F)
# build an empty file with column name to save analyzed result


for (i in 1:length(dir.name)) {
  # traverse all files(mice)
  
  fileM1_name <-  paste(path,dir.name[i],"trailsM1.csv",sep='/')
  fileSTR_name <-  paste(path,dir.name[i],"trailsSTR.csv",sep='/')
  data_1<- read.csv(fileM1_name,header = T)
  data_2<- read.csv(fileSTR_name,header = T)
  data_M1 <- data_1[1:min(nrow(data_1),nrow(data_2)),]; 
  data_STR <- data_2[1:min(nrow(data_1),nrow(data_2)),]; #Make sure the same length of data to be brought into correlation analysis below
  averaged_M1<-rowMeans(data_M1[,-ncol(data_M1)])
  averaged_STR<-rowMeans(data_STR[,-ncol(data_STR)])

  coef<- cor(averaged_M1,averaged_STR)
  # Pearson's correlation coefficient of M1 and STR signal sequence was obtained
  ccf_result <- ccf(averaged_M1, averaged_STR, lag.max=80, plot = F) 
  #lag.max was set to be 2s (fps was 40)
  lag_time <- ccf_result$lag[which.max(ccf_result$acf)]/40
  #phase lag was obtained in seconds (fps was 40)
  
  mice_number<-dir.name[i]
  analyze_result <- data.frame( mice_number, coef, lag_time )
  write.table(analyze_result,file="result.CSV", append = TRUE,sep = ",",row.names = F, col.names = F )

}

