# For drawing combined Z-score traces of M1 and STR Ca2+ activity
#  The data temporally aligned to behavior initiation was pre-processed 
#  by software (Inper Data Processing-v07.2) and then exported as Z-scored form in advance
#    By XRYao

library(readxl)
library(ggplot2)
path<-"Folder_PATH"
setwd(path)

######
data_M1<-read_excel("M1 File_PATH",sheet = 2)
data_STR<-read_excel("STR File_PATH",sheet = 2)

time<-data_M1$`Timestamp(ns)`/10^9; #Transfer default unit of time into sec
average_z<-data_M1$Mean;
SEM<-data_M1$SE;

time_STR<-data_STR$`Timestamp(ns)`/10^9;
average_zSTR<-data_STR$Mean;
SEM_STR<-data_STR$SE;


p<-ggplot()+ 
  geom_ribbon(data = data_M1, 
              aes(x = time,
                  y =average_z, 
                  ymin = average_z - SEM, 
                  ymax = average_z + SEM), 
              fill = "#ff0033",
              alpha = 0.3 )+ 
  #ribbon represents ¡ÀSE; set its color, opacity
  geom_line(data = data_M1, 
            aes(x = time,y = average_z ),
            color='#ff0033',
            size=0.25) +  
  #line represents averaged value; set its color, thickness
  geom_ribbon(data = data_STR, 
              aes(x = time_STR,
                  y =average_zSTR , 
                  ymin = average_zSTR - SEM_STR, 
                  ymax = average_zSTR + SEM_STR), 
              fill = "#72be64",
              alpha = 0.3 )+ 
  geom_line(data = data_STR, 
            aes(x = time_STR,
                y = average_zSTR ),
            color='#72be64',
            size=0.25) +  

  xlab(NULL)+ylab(NULL)+  
  theme_classic()+ 
  theme( axis.text = element_blank(),   
         axis.line =element_line(size=0.4),
         axis.ticks =element_line(size=0.4)    #set the theme of graph
  ) 

p<-p+
  ylim(-1.1, 1.25)+ #set y-axis scale
  scale_x_continuous(breaks=seq(-2, 5, 1))+
  coord_cartesian(xlim = c(-2,5)) 
  #set breaks and display range of x-axis 
print(p)

svg(file = "#Name.svg",   
    width =1.6, 
    height = 1.2,  
    #set image size (unit:inch) and save
)

print(p) #preview the image
dev.off()
