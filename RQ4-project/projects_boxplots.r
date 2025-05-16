# load packages
library(dplyr)
library(ggplot2)
setwd("/RQ4-project/dont_touch_this_folder_jimmy")

df <- read.csv("top250_stats.csv")


#collection date
date1 <- as.POSIXct("2024-05-28", tz = "UTC", format = "%Y-%m-%d")
#repo creation date
date2 <- as.POSIXct(df$creation_date, tz = "UTC", format = "%Y-%m-%d")
# Calculate the difference in days
df$days <- as.numeric(difftime(date1, date2, units = "days"))
df$X <- NULL

#View(df)
df[, 4:14] <- (df[, 4:14] / df$days)
df2 <- df

gathered_df <- gather(df2, attribute, count, -type, -tool_used, -link, -creation_date, -days)
gathered_df$link <- NULL
gathered_df$tool_used <- NULL
gathered_df$creation_date <- NULL

df <- gathered_df
df <- df %>%
  filter(attribute!="closedIssues") %>%
  filter(attribute!="openIssues") 

plot <- ggplot(df, aes(x=reorder(attribute, -(as.numeric(count))), y=log(as.numeric(count)), fill=type)) + 
  geom_boxplot() +
  ylab("Log of Count") + xlab("Github Repository Stats") +
  labs(fill = "Standard ") +
  theme(legend.position="top") +
  facet_wrap( ~ attribute, scales="free") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

plot

pdf(file = "../top250_metrics.pdf",width = 4, height = 3.5)
plot
dev.off()



# WILCOXON STARTS!

df <- gathered_df
names(df)[names(df) == "type"] <- "identifier"

df <- df %>%
  filter(count>=0) 
df <- na.omit(df)

class_spdx <- subset(df, identifier == "SPDX")
class_cdx <- subset(df, identifier == "CycloneDX")
# Perform paired Wilcoxon signed-rank test for each identifier
p_values <- list()
for (id in unique(df$attribute)) {
  cat("attribute:", id, "\n")
  
  # Subset data for the current identifier
  id_data_A <- as.numeric(subset(class_spdx, attribute == id)$count)
  id_data_B <- as.numeric(subset(class_cdx, attribute == id)$count)
  
  # Perform Wilcoxon signed-rank test
  #print(wilcox.test(id_data_A, id_data_B, paired = FALSE))
  
  mw_test <- wilcox.test(id_data_A, id_data_B, paired = FALSE)
  #cat("p-value:", mw_test$p.value, "\n")
  p_values[[id]] <- mw_test$p.value
  
  # Calculate Cliff's delta
  cliffs_delta <- function(x, y) {
    n1 <- sum(!is.na(x))
    n2 <- sum(!is.na(y))
    sum(ifelse(outer(x, y, "<"), 1, 0)) / (n1 * n2)
  }

  delta <- cliffs_delta(id_data_A, id_data_B)
  cat("cliff's delta:", delta, "\n")
  cat("\n")
}

# Convert p_values list to a vector
p_values_vec <- unlist(p_values)

# Adjust p-values using Bonferroni correction
adjusted_p_values <- p.adjust(p_values_vec, method = "bonferroni")

# Print adjusted p-values
for (i in 1:length(adjusted_p_values)) {
  cat("attribute:", names(adjusted_p_values)[i], "\n")
  cat("adjusted p-value: ", adjusted_p_values[i], "\n")
}

mean(df$count)
df3 <- df %>%
  group_by(identifier, attribute) %>%
  summarise(mean=mean(as.numeric(count)/100),
            median = median(as.numeric(count))/100)


