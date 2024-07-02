# For analyze the phenotype of licking behavior
#  Several txt files were stored in a folder. Each text is the licking record (0 or 1 with by-row time stamp)
#  form a mouse, named by the mouse number.
#   Several index of licking was calculated by the callable,customized function
#    The result was written in a csv file by row, with the mouse number as row name
#     By XRYao


library(stringr)
source("Function File_Path") #Load the R function "licking_phenotype_analyze"

path <- "Folder_Path" 
setwd(path)

row_name <- data.frame('miceNo.','invterval','duration','licks in bout','bout length')
write.table(row_name, file = "result.CSV", append = TRUE,sep = ",",row.names = F, col.names = F)
# Build an empty file with column name to save analyzed result


dir.name <- dir(path) 

file.name <- character()
for (i in 1:length(dir.name)) {
  file.name[i] <-  paste(path,dir.name[i],sep='\\') 
}


for (i in 1:length(dir.name)) {
  # Traverse all files(mice)
  
  number <- str_sub(string = file.name[i] ,start = -7,end = -5) 
  # Obtain the mouse number 
  licking_phenotype_analyze(file.name[i],number) 
  # Call the function
}


