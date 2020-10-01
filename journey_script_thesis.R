#install.packages("tidyverse") 
#install.packages("rstatix") 
#install.packages("ggpubr") 

library(tidyverse) 
library(rstatix)
library(ggpubr)
library(readxl)

theme_set(theme_bw())

# Reading in the data
star_jump_data <- read_excel("Star_jump_data.xlsx")
journey_wide <- star_jump_data %>% 
  rename(age=Age, gender=MF, fitness=Fitness, withMusic=Music) %>% 
  select(-Order)

# Reshaping the data
journey <- pivot_longer(journey_wide, -c(ID, age, gender, fitness, withMusic), names_to = "music", values_to = "stars") %>% 
  mutate(music = factor(music, levels = c("LM", "IM", "NM"))) %>%
  mutate(ID = as.factor(ID))


# Main analysis ---- 

# Summary statistics
journey %>%
  group_by(music) %>%
  get_summary_stats(stars, type = "full")

get_summary_stats(stars, type = "mean_sd")

# Visualisation

bxp <- ggplot(journey, aes(x = music, y = stars)) + 
  geom_boxplot(aes(fill = music),  alpha = 0.7, outlier.color = "#661100", outlier.size = 3, show.legend =  FALSE) + 
  geom_point(shape = 8) + 
  scale_fill_brewer(palette="Dark2")

bxp

# Outliers 
journey %>%
  group_by(music) %>%
  identify_outliers(stars)
# There were no extreme outliers.

# Check normality 
journey %>%
  group_by(music) %>%
  shapiro_test(stars)
# The number of jumps  was normally distributed at each IV level, as 
# assessed by Shapiro-Wilk’s test (p > 0.05).   


# QQ plots
ggqqplot(journey, "stars", facet.by = "music")

# ANOVA
journey.aov <- anova_test(data = journey, dv = stars, wid = ID, within = music)
get_anova_table(journey.aov)

# Difference between conditions significant

# Pairwise comparisons: t-tests 
journey.pwc <- journey %>%
  pairwise_t_test(
    stars ~ music, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
journey.pwc


# Add to visualisation
stat.test <- journey.pwc %>%
  mutate(y.position = c(250, 260, 270))

journey.pwc <- journey.pwc %>% add_xy_position(x = "music")
bxp + 
  stat_pvalue_manual(stat.test,) +
  scale_x_discrete(labels = c("Music with Lyrics", "Instrumental Music", "No Music")) + 
  labs(x = NULL, y = "Star Jumps", title = "Number of star jumps by music condition", subtitle = get_test_label(journey.aov, detailed = TRUE), caption = get_pwc_label(journey.pwc))



#Exploratory analysis of effect of age group ----
journey %>% 
  group_by(age) %>% 
  summarise(n=n()/3, mean(stars), sd(stars))

journey <- journey %>% 
  mutate(age_group = case_when(
    age %in% c("25-34", "35-44", "45-54") ~ ">25",
    age %in% c("18-24") ~ "<25"
  ))

journey <- 
  journey %>% 
  mutate(age_group = as.factor(age_group))

journey %>%
  group_by(age_group) %>%
  get_summary_stats(stars, type = "mean_sd")

journey %>%
  group_by(music, age_group) %>%
  get_summary_stats(stars, type = "mean_sd")

journey %>%
  group_by(music, age_group) %>%
  shapiro_test(stars)

journey %>%
  group_by(music) %>%
  levene_test(stars ~ age_group)

journey %>% 
  group_by(age_group) %>% 
  summarise(n(), mean(stars), sd(stars))
ggqqplot(journey, "stars", facet.by = "age_group")
ggplot(journey, aes(x = music, y = stars, fill = age_group)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(pch = 21, position = position_jitterdodge(jitter.width = 0.1)) + 
  scale_fill_brewer(palette="Dark2")

# Two-way mixed ANOVA test
age.aov <- anova_test(
  data = journey, dv = stars, wid = ID,
  between = age_group, within = music)
get_anova_table(age.aov)


# Exploratory analysis: fitness level ----

journey %>% 
  group_by(fitness) %>% 
  summarise(n(), mean(stars), sd(stars))

journey <- journey %>% 
  mutate(fitness = case_when(
    fitness %in% c(1) ~ "below_avg",
    fitness %in% c(2,3) ~ "avg+"
  ))

journey <- 
  journey %>% 
  mutate(fitness = as.factor(fitness))

journey %>%
  group_by(fitness) %>%
  get_summary_stats(stars, type = "mean_sd")

journey %>%
  group_by(music, fitness) %>%
  get_summary_stats(stars, type = "mean_sd")

journey %>%
  group_by(music, fitness) %>%
  shapiro_test(stars)

journey %>%
  group_by(music) %>%
  levene_test(stars ~ fitness)

ggqqplot(journey, "stars", facet.by = "fitness")


ggplot(journey, aes(x = music, y = stars, fill = fitness)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(pch = 21, position = position_jitterdodge(jitter.width = 0.1)) + 
  scale_fill_brewer(palette="Dark2")

# Two-way mixed ANOVA test
fitness.aov <- anova_test(
  data = journey, dv = stars, wid = ID,
  between = fitness, within = music)
get_anova_table(fitness.aov)


# Exploratory analysis: preference for listening to music while exercising ----
journey <- 
  journey %>% 
  mutate(withMusic = as.factor(withMusic))

journey %>%
  group_by(withMusic) %>%
  get_summary_stats(stars, type = "mean_sd")

journey %>%
  group_by(music, withMusic) %>%
  get_summary_stats(stars, type = "mean_sd")

journey %>%
  group_by(music, withMusic) %>%
  shapiro_test(stars)

journey %>%
  group_by(music) %>%
  levene_test(stars ~ withMusic)

journey %>% 
  group_by(withMusic) %>% 
  summarise(n(), mean(stars), sd(stars))
ggqqplot(journey, "stars", facet.by = "withMusic")


ggplot(journey, aes(x = music, y = stars, fill = withMusic)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(pch = 21, position = position_jitterdodge(jitter.width = 0.1)) + 
  scale_fill_brewer(palette="Dark2")

# Two-way mixed ANOVA test
withMusic.aov <- anova_test(
  data = journey, dv = stars, wid = ID,
  between = withMusic, within = music)
get_anova_table(withMusic.aov)


# Exploratory analysis: gender ----

journey <- 
  journey %>% 
  mutate(gender = as.factor(gender))

journey %>%
  group_by(gender) %>%
  get_summary_stats(stars, type = "mean_sd")

journey %>%
  group_by(music, gender) %>%
  get_summary_stats(stars, type = "mean_sd")

journey %>%
  group_by(music, gender) %>%
  shapiro_test(stars)

journey %>%
  group_by(music) %>%
  levene_test(stars ~ gender)

journey %>% 
  group_by(gender) %>% 
  summarise(n(), mean(stars), sd(stars))
ggqqplot(journey, "stars", facet.by = "gender")


ggplot(journey, aes(x = music, y = stars, fill = gender)) + 
  geom_boxplot(outlier.shape = NA) + 
  geom_point(pch = 21, position = position_jitterdodge(jitter.width = 0.1)) + 
  scale_fill_brewer(palette="Dark2")

# Two-way mixed ANOVA test
gender.aov <- anova_test(
  data = journey, dv = stars, wid = ID,
  between = gender, within = music)
get_anova_table(gender.aov)


# No evidence of interaction between the variables. 




