# For drawing dF/F0 traces of Ca2+ activity
#  The data temporally aligned to behavior initiation was pre-processed 
#  by software (Inper Data Processing-v07.2) and then exported as dF/F0 form in advance
#    By XRYao


path<-"Folder_Path"
setwd(path)
library(readxl)
library(ggplot2)

data<-read_excel("Data File_Path",sheet = 1)

time<-data$`Timestamp(ns)`/10^9; #Transfer default unit of time into sec
average_z<-data$Mean;
SEM<-data$SE;


p<-ggplot()+ 
  geom_ribbon(data = data, aes(x = time,y =average_z , ymin = average_z - SEM, ymax = average_z + SEM), 
              fill = "#ff0033",alpha = 0.3 )+
  #ribbon represents ¡ÀSE; set its color, opacity
  geom_line(data = data, aes(x = time,y = average_z ),color='#ff0033',size=1) + 
  #line represents averaged value; set its color, thickness
  theme_classic()+ 
  xlab(NULL)+ylab(NULL)+ 
  theme_classic()+ 
  scale_x_continuous(breaks=seq(-5, 5, 1))+
  scale_y_continuous(breaks=seq(-0.02, 0.6, 0.02))
  #set breaks of axis 

print(p)


svg(file = "#Name.svg",   
    width =3, 
    height =2,  
    #set image size and save

)


