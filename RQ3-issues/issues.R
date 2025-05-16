setwd("/RQ3-issues")
library("dplyr")

# Analysis on all issues for comparing formats:
cdx_issues = read.csv("CdxIssues.csv")
spdx_issues = read.csv("SpdxIssues.csv")

cdx_issues <- cdx_issues %>%
  filter(cdx_issues$Year>=2018) 
View(cdx_issues)

spdx_issues <- spdx_issues %>%
  filter(spdx_issues$Year>=2018) 
View(spdx_issues)

cdx_repos <- cdx_issues %>% 
  group_by(Repo) %>% 
  summarise(n = n())
View(cdx_repos)

spdx_repos <- spdx_issues %>% 
  group_by(Repo) %>% 
  summarise(n = n())
View(spdx_repos)

library(rstatix)
library(coin)
df3 = data.frame(value=c(spdx_repos$n,cdx_repos$n), 
                 type=c(rep("SPDX",length(spdx_repos$n)),
                          rep("CDX",length(cdx_repos$n))))

# Subset the data for SPDX and CycloneDx types
spdx_data <- df3[df3$type == "SPDX", "value"]
cyclonedx_data <- df3[df3$type == "CDX", "value"]
  
num_groups <- 2
num_comparisons <- num_groups * (num_groups - 1) / 2
alpha <- 0.05
adjusted_alpha <- alpha / num_comparisons

# Perform Mann-Whitney U test
mwu_test <- wilcox.test(spdx_data, cyclonedx_data, conf.level = 1 - adjusted_alpha)
# Calculate Cliff's delta
cliffs_delta <- function(x, y) {
  n1 <- sum(!is.na(x))
  n2 <- sum(!is.na(y))
  sum(ifelse(outer(x, y, "<"), 1, 0)) / (n1 * n2)
}
delta <- cliffs_delta(spdx_data, cyclonedx_data)
cat("cliff's delta:", delta, "\n")
cat("\n")

cdx_repos <- cdx_issues %>% 
  group_by(Repo,State) %>% 
  summarise(n = n())
View(cdx_repos)

spdx_repos <- spdx_issues %>% 
  group_by(Repo,State) %>% 
  summarise(n = n())
View(spdx_repos)

df3 = data.frame(value=c(spdx_repos$n,cdx_repos$n), 
                 type=c(rep("SPDX",length(spdx_repos$n)),
                        rep("CDX",length(cdx_repos$n))),
                 state=c(spdx_repos$State,cdx_repos$State)
                 )

library(boot)
mean_fun <- function(data, indices) {
  mean(data[indices, "value"])
}
bootstrap_results <- boot(df3 %>% filter((type == "SPDX")), mean_fun, R = 1000)
boot_means <- boot.ci(bootstrap_results, type = "basic")
print(boot_means)

bootstrap_results <- boot(df3 %>% filter((type == "CDX")), mean_fun, R = 1000)
boot_means <- boot.ci(bootstrap_results, type = "basic")
print(boot_means)

View(df3[df3$type=="SPDX" & df3$state=="open",]$value)

num_groups <- 2
num_comparisons <- num_groups * (num_groups - 1) / 2
alpha <- 0.05
adjusted_alpha <- alpha / num_comparisons

# Perform Mann-Whitney U test
mwu_test <- wilcox.test(df3[df3$type=="SPDX" & df3$state=="open","value"], 
                        df3[df3$type=="CDX" & df3$state=="open","value"], 
                        conf.level = 1 - adjusted_alpha)
print(mwu_test)
delta <- cliffs_delta(df3[df3$type=="SPDX" & df3$state=="open","value"], 
                      df3[df3$type=="CDX" & df3$state=="open","value"])
cat("cliff's delta:", delta, "\n")
cat("\n")

mwu_test <- wilcox.test(df3[df3$type=="SPDX" & df3$state=="closed","value"], 
                        df3[df3$type=="CDX" & df3$state=="closed","value"], 
                        conf.level = 1 - adjusted_alpha)
print(mwu_test)
delta <- cliffs_delta(df3[df3$type=="SPDX" & df3$state=="closed","value"], 
                      df3[df3$type=="CDX" & df3$state=="closed","value"])
cat("cliff's delta:", delta, "\n")
cat("\n")


mean(df3[df3$type=="SPDX" & df3$state=="open","value"])
sd(df3[df3$type=="SPDX" & df3$state=="open","value"])
mean(df3[df3$type=="CDX" & df3$state=="open","value"])
sd(df3[df3$type=="CDX" & df3$state=="open","value"])

mean(df3[df3$type=="SPDX" & df3$state=="closed","value"])
sd(df3[df3$type=="SPDX" & df3$state=="closed","value"])
mean(df3[df3$type=="CDX" & df3$state=="closed","value"])
sd(df3[df3$type=="CDX" & df3$state=="closed","value"])

# Merging the files of spdx and cdx issues
library(dplyr)
library(ggplot2)


# Add identifier column to df1 and df2
df1 <- mutate(cdx_issues, identifier = "CycloneDX")
df2 <- mutate(spdx_issues, identifier = "SPDX")

# Concatenate the data frames vertically
df <- bind_rows(df1, df2)
View(df)

df1 <- df %>% 
  group_by(Repo,State,identifier) %>% 
  summarise(n = n())
View(df1)


plot <- ggplot(df1, aes(x=df1$State, y=log(df1$n), fill=df1$identifier)) +
  geom_boxplot(position=position_dodge(1)) +
  ylab("Log of number of Issues") +
  xlab("Issue Status") +
  labs(fill="Standard") +
  scale_fill_manual(values=c("#999999", "#E69F00")) 

pdf(file = "cdx_vs_spdx_issues.pdf",width = 8, height = 3)
plot
dev.off()


df4 <- df1 %>% 
  group_by(identifier,State) %>% 
  summarise(sd = sd(n, na.rm = TRUE))
View(df4)


#####################
#####################
#####################
#####################
### RESOLUTION TIME 
#### CALCULATION ####
#####################
#####################
#####################
#####################

# Comparing the resolve time of each of the closed issues b/w formats
df1 <- df %>% 
  filter (df$State == "closed")
View(df1)

plot <- ggplot(df1, aes(x=df1$identifier, y=log(df1$time_diff), fill=df1$identifier)) +
  geom_violin(position=position_dodge(1)) +
  geom_boxplot(width=0.1) +
  ylab("Log of issue resolve time (sec)") +
  xlab("Standard") +
  theme(legend.position = "none") +
  scale_fill_manual(values=c("#999999", "#E69F00")) 

pdf(file = "cdx_vs_spdx_resolve_time.pdf",width = 8, height = 3)
plot
dev.off()


df4 <- df1 %>% 
  group_by(identifier) %>% 
  summarise(sd = sd(time_diff, na.rm = TRUE))
View(df4)


wilcox_result <- df1 %>% 
  rstatix::wilcox_test(time_diff ~ identifier,p.adjust.method = "bonferroni")
p_value <- wilcox_result$p
p_value
test_statistic <- wilcox_result$statistic
library(effsize)
cohen.d(df1$time_diff ~ df1$identifier)

df1 %>%  
  rstatix::wilcox_effsize(time_diff ~ identifier,p.adjust.method = "bonferroni")

cdx_time <- mean(df1[df1$identifier=="CycloneDX","time_diff"])
spdx_time <- mean(df1[df1$identifier=="SPDX","time_diff"])


(sd(df1[df1$identifier=="CycloneDX","time_diff"])/sd(df1[df1$identifier=="SPDX","time_diff"]))*100

cdx_time/spdx_time * 100

# ==============================================
# ==============================================
# ==============================================
# ==============================================
# ==============================================
# ==============================================
# Calculating the TREND of the past years for number of issues against resolution time.

all_issues <- df

issues_trend <- all_issues %>% 
  group_by(Year,identifier) %>% 
  filter((State == "closed")) %>%
  filter((Year != "2024")) %>%
  summarise(issues = n(), 
            avg_Resolution_Time = mean(time_diff, na.rm = TRUE))
View(issues_trend)

gathered_df <- gather(issues_trend, Key, Value, -Year, -identifier)
gathered_df$Concatenated <- paste(gathered_df$identifier, gathered_df$Key)
View(gathered_df)


# plot <- ggplot(gathered_df %>% 
#          filter((Key == "avg_Resolution_Time")), 
#        aes(group=identifier,
#            x=as.character(Year), 
#            y=log(Value), 
#            color=identifier)) + 
#   geom_line() + geom_point() +
#   ylab("Log of issue resolve time (sec)") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.9)) +
#   theme(legend.position = c(0.43, 0.11), legend.direction = "horizontal") +
#   xlab("Years") +
#   labs(colour="Format")

pdf(file = "cdx_vs_spdx_resolve_trend.pdf",width = 4, height = 3)
ggplot(gathered_df %>% 
         filter(Key == "avg_Resolution_Time", Value > 0),  # Ensure no zero values
       aes(group = identifier,
           x = as.character(Year), 
           y = log10(Value + 0.0001),  # Avoid log(0)
           color = identifier)) + 
  geom_line() + geom_point() +
  ylab("Log of issue resolve time (sec)") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9)) +
  theme(legend.position = c(0.43, 0.11), legend.direction = "horizontal") +
  xlab("Years") +
  labs(colour = "Format") +
  scale_y_continuous(trans = "log10")  # Ensure proper log scale
dev.off()


# plot <- ggplot(gathered_df %>% 
#                  filter((Key == "issues")), 
#                aes(group=identifier,
#                    x=as.character(Year), 
#                    y=(Value), 
#                    color=identifier)) + 
#   geom_line() + geom_point() +
#   ylab("Log of number of issues") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.9)) +
#   theme(legend.position = c(0.6, 0.11), legend.direction = "horizontal") +
#   xlab("Years") +
#   labs(colour="Format")


pdf(file = "cdx_vs_spdx_issues_trend.pdf",width = 4, height = 3)
ggplot(gathered_df %>% 
         filter(Key == "issues", Value > 0),  # Ensure no zero values
       aes(group = identifier,
           x = as.character(Year), 
           y = log10(Value + 0.0001),  # Avoid log(0)
           color = identifier)) + 
  geom_line() + geom_point() +
  ylab("Log of number of issues") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9)) +
  theme(legend.position = c(0.63, 0.11), legend.direction = "horizontal") +
  xlab("Years") +
  labs(colour = "Format") +
  scale_y_continuous(trans = "log10")  # Ensure proper log scale
dev.off()
