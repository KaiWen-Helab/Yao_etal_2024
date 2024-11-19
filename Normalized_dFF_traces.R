# Data was pre-processed by software (Inper Data Processing-v07.2)
# and then exported (xlsx format) in advance
#By XRYao

path<-"Path_of_folder"
setwd(path)
library(readxl)
library(ggplot2)

data_M1<-read_excel("Path_of_dataM1_file",sheet = 1)
data_STR<-read_excel("Path_of_dataSTR_file",sheet = 1)

#Input the data and perform the normalization
time<-data_M1$`Timestamp(ns)`/10^9;
norm_dFF_M1<-data_M1$Mean/max(data_M1$Mean);
SEM<-data_M1$SE/max(data_M1$Mean);

time_STR<-data_STR$`Timestamp(ns)`/10^9;
norm_dFF_STR<-data_STR$Mean/max(data_STR$Mean);
SEM_STR<-data_STR$SE/max(data_STR$Mean);

#Use the ggplot function, set the parameters of drawing 
p<-ggplot()+ 
  geom_ribbon(data = data_M1, aes(x = time,y =norm_dFF_M1 , ymin = norm_dFF_M1 - SEM, ymax = norm_dFF_M1 + SEM) , 
              fill = "#ff0033",alpha = 0.3 )+ 
  #ribbon represents ±SE; set its color, opacity
  geom_line(data = data_M1, aes(x = time,y = norm_dFF_M1 ),color='#ff0033',linewidth=0.25) + 
  #line represents averaged value; set its color, thickness
  geom_ribbon(data = data_STR, aes(x = time_STR,y =norm_dFF_STR , ymin = norm_dFF_STR - SEM_STR, ymax = norm_dFF_STR + SEM_STR) , 
              fill = "#72be64",alpha = 0.3 )+ 
  geom_line(data = data_STR, aes(x = time_STR,y = norm_dFF_STR ),color='#72be64',linewidth=0.25) + 
  xlab(NULL)+ylab(NULL)+  
  theme_classic()+ 
  ylim(-0.2, 1.2)+
  theme( axis.text = element_blank(),
         axis.line =element_line(linewidth=0.4),
         axis.ticks =element_line(linewidth=0.4),
         axis.ticks.length = unit(0.08, "cm"),
         plot.margin = unit(c(0, 0, 0, 0), "pt")  # Set the plot theme
        )+   
  scale_x_continuous(breaks=seq(-5, 5, 1))+
  scale_y_continuous(breaks=seq(0, 1, 1)) # Set the scale of axis

print(p)

#Save the plot of normalized dFF traces (svg format)
svg(file = "NormDff.svg",  
    width =1.2, 
    height =0.8,  
)


print(p)
dev.off()

#----Draw the raw dFF trace of STR data-------------

time_STR<-data_STR$`Timestamp(ns)`/10^9;
average_zSTR<-data_STR$Mean;
SEM_STR<-data_STR$SE;


p<-ggplot()+ 
  
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
            linewidth=0.25) +  
  xlab(NULL)+ylab(NULL)+  
  theme_classic()+ 
  theme( #
         axis.ticks.length = unit(0.05, "cm"),
         axis.line =element_line(linewidth=0.2),
         axis.ticks =element_line(linewidth=0.2),   
         plot.margin = unit(c(0, 0, 0, 0), "pt") 
  ) 

p<-p+
  scale_x_continuous(breaks=seq(-5, 5, 5))

print(p)


p<-p+theme( axis.text = element_blank())

svg(file = "STR-Dff.svg",   
    width =0.6, 
    height = 0.4,  
)

print(p)
dev.off()



#----Draw the raw dFF trace of M1 data-------------

time<-data_M1$`Timestamp(ns)`/10^9;
average_z<-data_M1$Mean;
SEM<-data_M1$SE;


p1<-ggplot()+ 
  geom_ribbon(data = data_M1, 
              aes(x = time,
                  y =average_z, 
                  ymin = average_z - SEM, 
                  ymax = average_z + SEM), 
              fill = "#ff0033",
              alpha = 0.3 )+ 
  geom_line(data = data_M1, 
            aes(x = time,y = average_z ),
            color='#ff0033',
            linewidth=0.25) +  
  xlab(NULL)+ylab(NULL)+  #
  theme_classic()+ 
  theme(axis.ticks.length = unit(0.05, "cm"),
        axis.line =element_line(linewidth=0.2),
        axis.ticks =element_line(linewidth=0.2),
        plot.margin = unit(c(0, 0, 0, 0), "pt") 
  ) 

p1<-p1+
  scale_x_continuous(breaks=seq(-5, 5, 5))
print(p1)

p1<-p1+theme( axis.text = element_blank())


svg(file = "M1-Dff.svg",   #保存的图片名
    width =0.6, #单位默认是inch,且不能改
    height = 0.4,  
    #分辨率默认300
)

print(p1)
dev.off()



