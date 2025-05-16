# load packages
library(dplyr)
library(ggplot2)


df1 <- read.csv("projects_using_oss_cdx.csv")
df2 <- read.csv("projects_using_oss_spdx.csv")

df <- rbind(df1, df2)
# prepare data for plotting
colnames(df)

df <- df %>%
  filter(attribute != 'commits')

df <- df %>%
  filter(attribute != 'packages')

df <- df %>%
  filter(attribute != 'usedBy')

df <- df %>%
  mutate(count = as.numeric(count))


plot <- ggplot(df, aes(x=reorder(attribute, -(as.numeric(count))), y=log(as.numeric(count)), fill=format)) +
  geom_boxplot() +
  ylab("Log of Count") + xlab("Github Repository Stats") +
  labs(fill = "Format ") +
  theme(legend.position="top") +
  facet_wrap( ~ attribute, scales="free") +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )
plot


df2 <- df %>%
  group_by(format, attribute) %>%
  summarise(median = median(as.numeric(count)))
df2

df3 <- df %>%
  group_by(format, attribute) %>%
  summarise(mean=mean(as.numeric(count)))
df3


df4 <- df %>%
  group_by(format, attribute) %>%
  summarise(sd=sd(as.numeric(count)))
df4
