# Function for analyze the phenotype of licking behavior
#  By XRYao

licking_phenotype_analyze<- function(fileName,number){
    
    data1<- read.table(fileName,header=F, fill=TRUE)
    
    trialendRow<-grep(pattern = "------",data1[,3])  #Find the row that trial ends
    trialendRow<-c(0,trialendRow)
    invterval<-c()
    duration<-c() #Duration of single lick within a trial
    totalNumber<-c() #Times of lick within a trial
    
    tiral_number<-length(trialendRow)-1  #Total times of trials
    
    for(i in 1:tiral_number ){
        row_strat<-trialendRow[i]+1
        row_end<-trialendRow[i+1]-1
        data_singleTrial<-data1[row_strat:row_end,]  
        #Extract the data of every trial
        
        absolute_time <- function(rowNumber){
            timepoint_stop<- unlist(strsplit(data_singleTrial[rowNumber,1],":" )) 
            sec<- as.numeric(timepoint_stop[3])
            min<- as.numeric(timepoint_stop[2])
            hr<- as.numeric(timepoint_stop[1])
            options(digits = 8)
            absolute_time<-round(hr*3600 + min*60 + sec, 3)
            return(absolute_time)
            
        }#Define a function getting the absolute time
    
        switch_stop_row1<-c()  #The row switching 0(no lick) to 1(lick)
        switch_begin_row1<-c()  #The row switching 1 to 0    
    
        absolute_time_stop<-c()
        absolute_time_begin<-c() 

        #Select the rows meeting the above criteria and record:
        for(i in 1:(nrow(data_singleTrial)-1) ){
            present_State<-as.numeric(data_singleTrial[i,3])
            next_State<-as.numeric(data_singleTrial[i+1,3])
            
            if(present_State==1 & next_State==0 ){
                switch_stop_row1 <- append(switch_stop_row1,i)
                absolute_time_stop<- append(absolute_time_stop, absolute_time(i))
            }
            
            if(present_State==0 & next_State==1 ){
                switch_begin_row1 <- append(switch_begin_row1,i+1)
                absolute_time_begin<- append(absolute_time_begin, absolute_time(i+1))
            }
        }
        
        interval_trial<-c()
        duration_trial<-c( absolute_time(switch_stop_row1[1])-absolute_time(1) )
        
        for (j in 1:length(absolute_time_begin)){
            options(digits = 5)
            interval_trial_single<-absolute_time_begin[j]-absolute_time_stop[j]
            interval_trial<-append(interval_trial,interval_trial_single)
        }
        # Calculate the interval between trials 
        
        for (j in 1:length(absolute_time_begin)){
            options(digits = 5)
            duration_trial_single<-absolute_time_stop[j+1]-absolute_time_begin[j]
            duration_trial<-append(duration_trial,duration_trial_single)
        }
        # Calculate the trial duration
        
        invterval<- append( invterval,mean(interval_trial) ) 
        duration <- append( duration,mean(na.omit(duration_trial)) )
        totalNumber <- append( totalNumber,length(absolute_time_stop) )
        
        miceNo. <- number
        analyze_result <- data.frame(miceNo., invterval, duration, totalNumber )
        write.table(analyze_result,file="result.CSV", append = TRUE,sep = ",",row.names = F, col.names = F )
        # Write the result in a csv file by row
    } 
}
