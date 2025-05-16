setwd("/RQ3-issues")
library("dplyr")

# Analysis on all issues for comparing formats:
cdx_issues = read.csv("CdxIssues.csv")
spdx_issues = read.csv("SpdxIssues.csv")

cdx_issues <- cdx_issues %>%
  filter(cdx_issues$Year>=2018) 

spdx_issues <- spdx_issues %>%
  filter(spdx_issues$Year>=2018) 


# Merging the files of spdx and cdx issues
library(dplyr)
library(ggplot2)


# Add identifier column to df1 and df2
df1 <- mutate(cdx_issues, identifier = "CycloneDX")
df2 <- mutate(spdx_issues, identifier = "SPDX")

# Concatenate the data frames vertically
df <- bind_rows(df1, df2)
library(lubridate)

###################


all_issues <- df

issues_trend <- all_issues %>%
  mutate(
    Year = year(as.Date(closed_at)),
    Month = month(as.Date(closed_at), label = TRUE, abbr = TRUE)  # e.g., "Apr"
  ) %>%
  group_by(Year, Month, identifier) %>%
  filter(State == "closed") %>%
  summarise(
    issues = n(),
    avg_Resolution_Time = mean(time_diff, na.rm = TRUE),
    .groups = "drop"
  )

issues_trend <- issues_trend %>%
  mutate(closed_at = paste(.issues_trend$month, .issues_trend$year))



gathered_df <- gather(issues_trend, Key, Value, -Month, -Year, -identifier)
gathered_df$Concatenated <- paste(gathered_df$identifier, gathered_df$Key)
gathered_df$closed_at <- make_date(gathered_df$Year,gathered_df$Month)


# Get every nth closed_at value for x-axis labels
every_nth <- function(x, n) {
  x[seq(1, length(x), by = n)]
}

# Extract unique x values and select every 4th (adjust as needed)
x_labels <- unique(as.character(gathered_df$closed_at))
x_labels_thinned <- every_nth(x_labels, 4)

# Now plot with only selected x-axis labels
pdf(file = "cdx_vs_spdx_resolve_trend_2024.pdf",width = 6, height = 6)
ggplot(gathered_df %>% 
         filter(Key == "avg_Resolution_Time", Value > 0),
       aes(group = identifier,
           x = as.character(closed_at), 
           y = log10(Value + 0.0001),
           color = identifier)) + 
  geom_line() + geom_point() +
  ylab("Log of issue resolution time (sec)") +
  xlab("Date") +
  labs(colour = "Format") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.9)) +
  theme(legend.position = c(0.43, 0.11), legend.direction = "horizontal") +
  scale_y_continuous(trans = "log10") +
  scale_x_discrete(breaks = x_labels_thinned)
dev.off()


# Now plot with only selected x-axis labels
pdf(file = "cdx_vs_spdx_issues_trend_2024.pdf",width = 6, height = 6)
ggplot(gathered_df %>% 
         filter(Key == "issues", Value > 0),
       aes(group = identifier,
           x = as.character(closed_at), 
           y = log10(Value + 0.0001),
           color = identifier)) + 
  geom_line() + geom_point() +
  ylab("Log of number of issues") +
  xlab("Date") +
  labs(colour = "Format") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.9)) +
  theme(legend.position = c(0.43, 0.11), legend.direction = "horizontal") +
  scale_y_continuous(trans = "log10") +
  scale_x_discrete(breaks = x_labels_thinned)
dev.off()

