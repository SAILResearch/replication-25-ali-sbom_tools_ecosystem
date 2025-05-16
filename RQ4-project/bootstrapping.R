# Load necessary libraries
library(dplyr)

# Function to perform bootstrapping and calculate the mean with 95% confidence interval
bootstrap_mean <- function(data) {
  # Set the number of bootstrap samples
  n_bootstraps <- 1000
  
  # Create an empty dataframe to store bootstrapped means and confidence intervals
  bootstrapped_results <- data.frame(attribute = character(), mean_count = numeric(), lower_ci = numeric(), upper_ci = numeric(), stringsAsFactors = FALSE)
  
  # Perform bootstrapping for each attribute
  for (attr in unique(data$attribute)) {
    # Filter data for the current attribute
    attr_data <- data %>%
      filter(attribute == attr) %>%
      select(count)
    
    # Create an empty vector to store bootstrapped means
    bootstrapped_attr_means <- numeric(n_bootstraps)
    
    # Perform bootstrapping
    for (i in 1:n_bootstraps) {
      # Resample with replacement from the original data
      bootstrap_sample <- sample(attr_data$count, replace = TRUE)
      # Calculate the mean for the bootstrap sample
      bootstrapped_attr_means[i] <- mean(bootstrap_sample)
    }
    
    # Estimate the true mean
    true_mean <- mean(bootstrapped_attr_means)
    
    # Calculate the 95% confidence interval
    lower_ci <- quantile(bootstrapped_attr_means, 0.025)
    upper_ci <- quantile(bootstrapped_attr_means, 0.975)
    
    # Store the attribute, its mean, and confidence intervals in the dataframe
    bootstrapped_results <- rbind(bootstrapped_results, data.frame(attribute = attr, mean_count = true_mean, lower_ci = lower_ci, upper_ci = upper_ci))
  }
  
  return(bootstrapped_results)
}

#df<-df1
#names(df)[names(df) == "State"] <- "attribute"
#names(df)[names(df) == "n"] <- "count"
# Perform bootstrapping for each group (identifier)
bootstrapped_results <- df1 %>%
  group_by(identifier) %>%
  do(bootstrap_mean(.))



# Output bootstrapped results
print(bootstrapped_results)
