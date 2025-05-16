# load packages
library(dplyr)
library(ggplot2)
setwd("/RQ4-project/dont_touch_this_folder_jimmy")

df <- read.table("commit_count_quartile.csv",
                 sep= ",",header=TRUE,row.names=1,as.is=TRUE)

library("gplots")
contingency_table <- as.matrix(df)
contingency_table

chi_square_result <- chisq.test(contingency_table)

chi_square_statistic <- chi_square_result$statistic
# Calculate Cramer's V manually
n <- sum(contingency_table)  # Total number of observations
num_rows <- nrow(contingency_table)
num_cols <- ncol(contingency_table)

# Calculate Cramer's V
cramers_v <- sqrt(chi_square_statistic / (n * min((num_rows - 1), (num_cols - 1))))

# Print Cramer's V
print("This is the Cramer's effect size: ")
print(cramers_v)

library(corrplot)

pdf(file = "commit_quartiles.pdf",width = 8, height = 3)
cp <- corrplot(chi_square_result$residuals, is.cor = FALSE,tl.col="black")
dev.off()

# Calculating Associations
View(round(chi_square_result$residuals, 3))

# Calculating Contributions
contrib <- 100*chi_square_result$residuals^2/chi_square_result$statistic
corrplot(contrib, is.cor = FALSE)
View(round(contrib, 3))
