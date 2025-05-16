
library(dplyr)
library(ggplot2)
setwd('/RQ4-project')

df1 <- read.csv("CycloneDX_languages.csv")
df2 <- read.csv("SPDX_languages.csv")

df <- rbind(df1, df2)

# prepare data for plotting
colnames(df)

df2 <- df[(df$language!='Dockerfile') & (df$language!='Batchfile') & (df$language!='Jinja')
          & (df$language!='Makefile') & (df$language!='XSLT') & (df$language!='YAML')
          & (df$language!='Jupyter') & (df$language!='HTML') & (df$language!='Vue'),]

df<-df2
# static_languages<-c(Java,TypeScript,C#,Rust,Kotlin,Swift,Scala,C++)
#dynamic_languages<-c("Go","Python","Shell","JavaScript","HTML","Vue","Ruby","Jupyter","Vim","Nix","Groovy","YAML","XSLT","PHP","Jinja","Erlang","Batchfile","PowerShell","Makefile")


language_frequency <- df %>%
  group_by(format,language) %>%
  summarise(Frequency = n())

language_frequency

#static_languages_frequency <- language_frequency[!(language_frequency$language %in% dynamic_languages), ]
#dynamic_languages_frequency <- language_frequency[language_frequency$language %in% dynamic_languages, ]

plotting_df <-
  language_frequency %>% 
  group_by(language, format) %>% 
  # a trick! for making a horizontal double sided bar plot
  mutate(Frequency = if_else(format == "SPDX", -Frequency, Frequency))
## find the order
temp_df <-
  plotting_df %>% 
  filter(format == "CycloneDX") %>% 
  arrange(Frequency)

the_order <- temp_df$language

plotting_df$hwidth <- ifelse(plotting_df$Frequency > 0, -0, 1)

# plot
p1 <- ggplot(plotting_df, aes(x = language, y = Frequency, group = format, fill = format)) +
  geom_bar(stat = "identity", width = 0.6) +
  coord_flip() +
  scale_x_discrete(limits = the_order) +
  geom_text(aes(label =  abs(Frequency)), hjust = plotting_df$hwidth,  size = 2.7) +
  # another trick!
  #scale_y_continuous(breaks = seq(-2000000, 2000000, 20000), 
  #                   labels = abs(seq(-2000000, 2000000, 20000))) +
  labs(x = "Language", y = "Frequency       ") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_text(size = 10.5), 
        axis.title.y = element_text(size = 10.5)) +
  scale_y_continuous(breaks = seq(-80, 80, 20), 
                     labels = abs(seq(-80, 80, 20)))

p1
pdf(file = "tool_languages.pdf",8.3,5)
p1
dev.off()

####################### Dynamically typed plot ##########################
plotting_df <-
  dynamic_languages_frequency %>% 
  group_by(language, format) %>% 
  # a trick! for making a horizontal double sided bar plot
  mutate(Frequency = if_else(format == "SPDX", -Frequency, Frequency))
## find the order
temp_df <-
  plotting_df %>% 
  filter(format == "CycloneDX") %>% 
  arrange(Frequency)

the_order <- temp_df$language

plotting_df$hwidth <- ifelse(plotting_df$Frequency > 0, -0, 1)

# plot
p2 <- ggplot(plotting_df, aes(x = language, y = Frequency, group = format, fill = format)) +
  geom_bar(stat = "identity", width = 0.6) +
  coord_flip() +
  scale_x_discrete(limits = the_order) +
  geom_text(aes(label =  abs(Frequency)), hjust = plotting_df$hwidth,  size = 2.7) +
  # another trick!
  #scale_y_continuous(breaks = seq(-2000000, 2000000, 20000), 
  #                   labels = abs(seq(-2000000, 2000000, 20000))) +
  labs(x = "Languages", y = "Frequency       ") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_text(size = 10.5), 
        axis.title.y = element_text(size = 10.5)) +
  scale_y_continuous(breaks = seq(-80, 80, 20), 
                     labels = abs(seq(-80, 80, 20)))

p2

############################# Paradigm Categorization #################

categorize_language <- function(language) {
  procedural_languages <- c('C', 'C#', 'C++', 'Go', 'Rust', 'Java', 'Kotlin', 'Swift', 'Scala')
  scripting_languages <- c('Python', 'Shell', 'JavaScript', 'Ruby', 'PHP', 'Groovy', 'TypeScript')
  functional_languages <- c('Haskell', 'Erlang')
  
  if (language %in% procedural_languages) {
    return('Procedural Languages')
  } else if (language %in% scripting_languages) {
    return('Scripting Languages')
  } else {
    return('Other')
  }
}

df <- df %>%
  group_by(format,language) %>%
  summarise(Frequency = n())

# Apply categorization to the DataFrame
df$category <- sapply(df$language, categorize_language)

df <- subset(df, category != 'Other')

df <- subset(df, category == 'Procedural Languages')
#df <- subset(df, category == 'Scripting Languages')

plotting_df <-
  df %>% 
  group_by(language, format) %>% 
  # a trick! for making a horizontal double sided bar plot
  mutate(Frequency = if_else(format == "SPDX", -Frequency, Frequency))
## find the order
temp_df <-
  plotting_df %>% 
  filter(format == "CycloneDX") %>% 
  arrange(Frequency)

the_order <- temp_df$language

plotting_df$hwidth <- ifelse(plotting_df$Frequency > 0, -0, 1)

# plot
p2 <- ggplot(plotting_df, aes(x = language, y = Frequency, group = format, fill = format)) +
  geom_bar(stat = "identity", width = 0.6) +
  coord_flip() +
  scale_x_discrete(limits = the_order) +
  geom_text(aes(label =  abs(Frequency)), hjust = plotting_df$hwidth,  size = 2.7) +
  # another trick!
  #scale_y_continuous(breaks = seq(-2000000, 2000000, 20000), 
  #                   labels = abs(seq(-2000000, 2000000, 20000))) +
  labs(x = "Languages", y = "Frequency       ") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x = element_text(size = 10.5), 
        axis.title.y = element_text(size = 10.5)) +
  scale_y_continuous(breaks = seq(-80, 80, 20), 
                     labels = abs(seq(-80, 80, 20)))

p2


############################## SUMMARY #############################

summary_stats <- language_frequency %>%
  group_by(format) %>%
  summarise(mean_frequency = mean(Frequency),
            median_frequency = median(Frequency),
            max_frequency = max(Frequency),
            min_frequency = min(Frequency),
            std_frequency = sd(as.numeric(Frequency)),
            total_projects = n())

print(summary_stats)
