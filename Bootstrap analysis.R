library(boot)
setwd("PATH_of_folder")


fileSTR_name <- "PATH_of_datafile"
data<- read.csv(Name_of_datafile,header = T)

#Separate data into the pre- and post-event stages
data_beofre <- data[1:(nrow(data)/2),]
data_after <- data[(nrow(data)/2):nrow(data),]

data_beofre <- colMeans(data_beofre)
data_after <- colMeans(data_after)

#-----------------
  
# Define a Bootstrap function
  bootstrap_paired_diff <- function(data1, data2, n = 1000) {
    mean_diffs <- numeric(n)
    data_before <- numeric(n)
    data_after <- numeric(n)
    
    for (i in 1:n) {
      # paired samples(pre- and post-event data) were sampled with replacement
      indices <- sample(1:length(data1), replace = TRUE)
      sample1 <- data1[indices]
      sample2 <- data2[indices]
      
      # Calculate the mean of two sets of sample
      data_before[i] <- mean(sample1)
      data_after[i] <- mean(sample2)
      
      # Calculate the difference between the means of two samples
      mean_diffs[i] <- data_before[i] - data_after[i]
    }
    
    # Calculate the 95% confidence interval
    ci <- quantile(mean_diffs, probs = c(0.025, 0.975))
    
    # Returns a list of two sets of sample means and their difference, confidence interval
    return( list(sample_before = data_before, sample_after = data_after, mean_diff = mean_diffs, ci=ci ) )
  }

#-----------------

# Invoking the Bootstrap function
bootstrap_results <- bootstrap_paired_diff(data_beofre, data_after)

# Single sample t test was performed
t_test_result <- t.test(bootstrap_results$mean_diff, mu = 0)

print(bootstrap_results$ci)

# Save the analyzed result and write in 
analyze_result <- data.frame( bootstrap_results$sample_before, bootstrap_results$sample_after )
write.table(analyze_result,file="result.CSV", append = TRUE,sep = ",",row.names = F, col.names = T )
