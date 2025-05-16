# Analysis on all issues for comparing issue resolve time:


# For extracted issue tags:
setwd("/RQ3-issues")
cdx_issues = read.csv("cdx_issues_with_tags.csv")
spdx_issues = read.csv("spdx_issues_with_tags.csv")

cdx_issues$year <- as.POSIXct(cdx_issues$Created.At, tz = "UTC", format = "%Y-%m-%d")
cdx_issues$year <- format(cdx_issues$year, "%Y")

spdx_issues$year <- as.POSIXct(spdx_issues$Created.At, tz = "UTC", format = "%Y-%m-%d")
spdx_issues$year <- format(spdx_issues$year, "%Y")

View(cdx_issues)

cdx_issues <- cdx_issues %>%
  filter(cdx_issues$year>=2018) 
View(cdx_issues)

spdx_issues <- spdx_issues %>%
  filter(spdx_issues$year>=2018) 
View(spdx_issues)
 
# Add identifier column to df1 and df2
df1 <- mutate(cdx_issues, identifier = "CycloneDX")
df2 <- mutate(spdx_issues, identifier = "SPDX")

# Concatenate the data frames vertically
df <- bind_rows(df1, df2)
View(df)
#write.csv(df,file="tags.csv")

View(cdx_issues)
View(spdx_issues)

cdx_repos <- cdx_issues %>% 
  group_by(Repo) %>% 
  summarise(n = n())
View(cdx_repos)

spdx_repos <- spdx_issues %>% 
  group_by(Repo) %>% 
  summarise(n = n())
View(spdx_repos)

cdx_tags <- cdx_issues %>% 
  group_by(Tags) %>% 
  summarise(n = n())
View(cdx_tags)

spdx_tags <- spdx_issues %>% 
  group_by(Tags) %>% 
  summarise(n = n())
View(spdx_tags)


tags <- df %>% 
  group_by(Tags,identifier) %>% 
  summarise(n = n())
View(tags)
#write.csv(tags,"tags_sum.csv")

summary(cdx_tags$n)
summary(spdx_tags$n)

cats <- read.csv("categories.csv")
View(cats)


df2 <- merge(df, cats[c('Tags', 'identifier', 'category')], by = c('Tags', 'identifier'), all.x = TRUE) %>% 
        filter(!is.na(category))

# spdx <- df2 %>% 
#   filter(identifier == "CycloneDX")
# View(spdx)
# unique(spdx$Repo)
# 
# test <- df2 %>% 
#   group_by(df2$category,df2$identifier,df2$Repo) %>% 
#   summarise(n = n())
# View(test)


View(df2)

# Tags prevalence calculation
t <- df2 %>% 
  group_by(identifier) %>% 
  summarise(n = n())
View(t)

categories <- df2 %>% 
  group_by(category,identifier) %>% 
  summarise(n = n())

categories <- categories %>%
  mutate(total = case_when(
    identifier == "SPDX" ~ 9027,
    identifier == "CycloneDX" ~ 16323
  ))

View(categories)
categories$percentage <- (categories$n/categories$total) * 100

# Doing this to create order :(
#write.csv(categories, file = "aggregatedCats.csv")
categories = read.csv("aggregatedCats.csv")

categories$category <- factor(categories$category, levels = rev(unique(categories$category)))

plot<- ggplot(categories, aes(x=category, y=ifelse(identifier == "CycloneDX", percentage, -percentage), fill=identifier)) + 
  geom_bar(stat="identity", position="identity") +
  scale_y_continuous(limits = c(-max(categories$percentage), max(categories$percentage))) +
  theme(legend.position = "top", legend.direction = "horizontal") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_text(
    size = 2.5,
    aes(
      y = ifelse(identifier == "CycloneDX", percentage-0.5 , -(percentage - 0.2)),
      fill = identifier,
      label = paste0(round(percentage, digits = 2), "%"),
      hjust = ifelse(identifier == "CycloneDX", -0.1, 1.1)  # Adjust hjust based on identifier
    ),
    vjust = 0.5
  ) +
  coord_flip() +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  ylab("Total Percentage of Issues") +
  xlab("Issue Categories") +
  labs(fill="SBOM Tool Standard")

pdf(file = "tags_prevalence.pdf",width = 8.2, height = 3.5)
plot
dev.off()

df3 <- df2 %>% 
  group_by(category,identifier) %>% 
  summarise(n = n())
View(df3)

library(coin)
df_cyclone <- subset(df3, identifier == "CycloneDX")
df_spdx <- subset(df3, identifier == "SPDX")

num_groups <- 14
num_comparisons <- num_groups * (num_groups - 1) / 2
alpha <- 0.05
adjusted_alpha <- alpha / num_comparisons

# Perform Mann-Whitney U test
wilcox.test(df_cyclone$n, df_spdx$n, conf.level = 1 - adjusted_alpha)


df2 %>% 
  filter((State == "closed")) %>% 
  rstatix::wilcox_test(resolve_time_sec ~ identifier,p.adjust.method = "bonferroni")

df2 %>% 
  filter((State == "closed")) %>% 
  rstatix::wilcox_effsize(resolve_time_sec ~ identifier,p.adjust.method = "bonferroni")

res.aov <- aov(resolve_time_sec ~ identifier, data = df2)
summary(res.aov)

df2 %>% 
  filter((State == "closed")) %>% 
  rstatix::wilcox_test(resolve_time_sec ~ category,p.adjust.method = "bonferroni")

t <- df2 %>% 
  filter((State == "closed"))

df5 <- data.frame(identifier=t$identifier,
                  time=t$resolve_time_sec,
                  category=t$category)

class_spdx <- subset(df5, identifier == "SPDX")
class_cdx <- subset(df5, identifier == "CycloneDX")

cliffs_delta <- function(x, y) {
  n1 <- sum(!is.na(x))
  n2 <- sum(!is.na(y))
  sum(ifelse(outer(x, y, "<"), 1, 0)) / (n1 * n2)
}

# Perform paired Wilcoxon signed-rank test for each identifier
for (id in unique(df5$category)) {
  cat("category:", id, "\n")
  
  # Subset data for the current identifier
  id_data_A <- subset(class_spdx, category == id)$time
  id_data_B <- subset(class_cdx, category == id)$time
  
  # Perform Wilcoxon signed-rank test
  print(wilcox.test(id_data_A, id_data_B, paired = FALSE))
  #cohen_d <- cohen.d(id_data_A, id_data_B, paired = FALSE)
  delta <- cliffs_delta(id_data_A, id_data_B)
  print(delta)
  cat("\n")
}





# ggplot(categories, aes(x = as.character(category), y = percentage)) +
#   geom_bar(aes(fill = identifier), position = "dodge", stat = "identity") +
#   ylab("% Number of Tools") +
#   xlab("NTIA Taxonomy Usecases") +
#   labs(fill = "Type of Tool") +
#   #theme(legend.position = c(0.48, 0.98), legend.direction = "horizontal") +
#   geom_text(size = 3, position = position_dodge(1), aes(y = percentage + 0.25, fill = identifier, label = paste0(round(categories$percentage, digits = 2), "%"), hjust = 0.5, vjust = 0)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#   scale_fill_manual(values = c("#999999", "#E69F00")) +
#   #coord_flip() +
#   facet_wrap(~ identifier, scales = "free")

# Resolution time claculation
t1 <- df2 %>% 
  group_by(category,identifier) %>% 
  filter((State == "closed")) %>%
  summarise(avg_Resolution_Time = mean(resolve_time_sec, na.rm = TRUE))

mean_time <- t1 %>% 
  group_by(identifier) %>% 
  summarise(total_time = sum(avg_Resolution_Time, na.rm = TRUE))
View(mean_time)

t1 <- t1 %>%
  mutate(total = case_when(
    identifier == "SPDX" ~ 217374489,
    identifier == "CycloneDX" ~ 192916931
  ))

t1$total <- t1$avg_Resolution_Time

#write.csv(t1, file = "aggregatedTagTime.csv")
t1 = read.csv("aggregatedTagTime.csv")
t1$category <- factor(t1$category, levels = rev(unique(categories$category)))

plot <- ggplot(t1, aes(x=category, y=ifelse(identifier == "CycloneDX", avg_Resolution_Time, -avg_Resolution_Time), fill=identifier)) + 
  geom_bar(stat="identity", position="identity") +
  scale_y_continuous(limits = c(-max(t1$avg_Resolution_Time), max(t1$avg_Resolution_Time))) +
  theme(legend.position = "top", legend.direction = "horizontal") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_text(
    size = 2.5,
    aes(
      y = ifelse(identifier == "CycloneDX", avg_Resolution_Time - 0.5, -(avg_Resolution_Time - 0.2)),
      label = paste0(round(avg_Resolution_Time / 86400, digits = 2), " days"),
      hjust = ifelse(identifier == "CycloneDX", -0.1, 1.1)  # Adjust hjust based on identifier
    ),
    vjust = 0.5
  ) +
  coord_flip() +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  ylab("Resolution Time (seconds)") +
  xlab("Issue Categories") +
  labs(fill="Type of Tool")

pdf(file = "tag_resolve_time.pdf",width = 9, height = 5)
plot
dev.off()

df2 %>% 
  filter((State == "closed")) %>% 
  rstatix::wilcox_test(resolve_time_sec ~ identifier,p.adjust.method = "bonferroni")

df2 %>% 
  filter((State == "closed")) %>% 
  rstatix::wilcox_effsize(resolve_time_sec ~ identifier,p.adjust.method = "bonferroni")

res.aov <- aov(resolve_time_sec ~ identifier, data = df2)
summary(res.aov)


##############################################################################
##############################################################################
##############################################################################
##############################################################################
# Subset the data for SPDX and CycloneDx types
df3 <- df2%>% 
  filter((State == "closed"))
spdx_data <- df3[df3$identifier == "SPDX", "resolve_time_sec"]
cyclonedx_data <- df3[df3$identifier == "CDX", "resolve_time_sec"]

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
