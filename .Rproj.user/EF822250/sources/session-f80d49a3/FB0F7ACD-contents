# title: "Lesson 3: Workflow and Statistical Test in R"
# author: Tim Leavy
# date: 2025-07-17

#loading packages
#arrow = reading in parquet files
library(arrow)


#tidyverse is for data manipulation
library(tidyverse)

#loading dataset

merged_data <- read_parquet(file = "merged_data.parquet", stringAsFactors = TRUE)

#Initial Exploration with Visualization

boxplot(formula = upward_mobility_rate_2010 ~ STATE_NAME_2010SVI, data = merged_data)

#Identify and Handle Anomalies. The anomaly being this 06 thing

merged_data <- merged_data[!(merged_data$STATE_NAME_2010SVI %in% "06"),]
#confirming the issues are resolved - here, we overwrote a subset of merged data - the 6 thing is gone in the second plot. 
#Basically we told it to take merged data, create a new thing called merged data but without the 6
# ! tells it to NOT put something in.
boxplot(formula = upward_mobility_rate_2010 ~ STATE_NAME_2010SVI, data = merged_data)

#create another boxplot to explore total population by state

boxplot(formula = M_TOTPOP_2010SVI ~ STATE_NAME_2010SVI, data = merged_data)

# what does this new plot tell you?
#
#Why did you choose this variable to explore next?
#
#Are there patterns worth investigating further?
#

boxplot(formula = M_TOTPOP_2010SVI ~ STATE_NAME_2010SVI, data = merged_data)

#Creating two new datasets by filtering the raws based on the state abbr
subset_data_ca <- subset(merged_data, STATE_ABBR_2010SVI == "CA")
subset_data_az <- subset(merged_data, STATE_ABBR_2010SVI == "AZ")

print(upward_mean_2010_ca <- mean(subset_data_ca$upward_mobility_rate_2010))
print(upward_mean_2010_az <- mean(subset_data_az$upward_mobility_rate_2010))

#storing the means in a new data frame so they're easier to use

upward_mobility_means_2010 <- data.frame(State = c("CA", "AZ"),up_2010_mean = c(upward_mean_2010_ca, upward_mean_2010_az))

#ok, so doing it the base r way sucks and is tedious
#we need to group the full dataset by state abbr, then calculate the mean upward mobility rate for each group

state_group <- merged_data %>% group_by(STATE_ABBR_2010SVI)
state_mob_means <- state_group %>% summarise(up_mean = mean(upward_mobility_rate_2010, na.rm = TRUE))

state_mob_means <- state_mob_means %>% filter(!is.na(STATE_ABBR_2010SVI))

upward <- merged_data %>%
  group_by(STATE_ABBR_2010SVI) %>%
  summarise(means_2010 = mean(upward_mobility_rate_2010, na.rm = TRUE))

upward <- upward %>% filter(!is.na(STATE_ABBR_2010SVI))

upward_stats <- merged_data %>%
  group_by(STATE_ABBR_2010SVI, COUNTY_2010SVI) %>%
  summarise(up_means = mean(upward_mobility_rate_2010, na.rm = TRUE),
            up_se = sd(upward_mobility_rate_2010, na.rm = TRUE)/sqrt(n()))

#Adding error bars using ggplot

ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means))

ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point()

ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() + 
  geom_errorbar(mapping = aes(ymin = up_means - up_se,
                              ymax = up_means + up_se))

ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) + 
  geom_point() +
  geom_errorbar(mapping = aes(ymin = up_means - up_se, ymax = up_means + up_se), width = 0.30)

upward_stats_st <- merged_data %>%
  group_by(STATE_ABBR_2010SVI) %>%
  summarize(up_means = mean(upward_mobility_rate_2010),
            up_se = sd(upward_mobility_rate_2010)/sqrt(n()))

#Dropping the NA

upward_stats_st <- upward_stats_st %>% filter(!is.na(STATE_ABBR_2010SVI))

#Redo the graph

ggplot(data = upward_stats_st, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() + 
  geom_errorbar(mapping = aes(ymin = up_means - up_se,
                              ymax = up_means + up_se), 
                width = 0.30)

#tidyverse tricks 3.6

merged <- merged_data

merged %>% 
  dplyr:: select(!contains("_"), starts_with ("upward"))


#Reordering columns - relocates columns containing "STATE" after the 2020 um rate

merged <- merged %>% dplyr:: relocate(contains("STATE"), .after = upward_mobility_rate_2020)

# unique ids for column

unique_id <- merged %>% dplyr:: group_by(STATE_FIPS_2010SVI, CENSUSAREA_2010SVI) %>% 
  dplyr:: mutate(uniqueid = row_number(),
                 .before = contains("_"))

#summarizing across multiple columns - summarizing stats across columns

merged_stats_up <- merged %>%
  dplyr::group_by(STATE_ABBR_2010SVI) %>%
  dplyr::summarize(across(starts_with("upward"), list(~mean(.x, na.rm = TRUE), ~sd(.x, na.rm = TRUE))))
                   
merged_stats_up <- merged %>% 
  dplyr::group_by(STATE_ABBR_2010SVI) %>% 
  dplyr::summarize(across(starts_with("upward"), 
                          list(mean = ~mean(.x, na.rm = TRUE), 
                               sd = ~sd(.x, na.rm = TRUE)),
                          .names = "{gsub('_', '', col)}_{fn}"))

#modeling and filtering NAs

upward_models <- merged %>% filter(!is.na(STATE_ABBR_2010SVI),
  !is.na(upward_mobility_rate_2010), !is.na(POP2010)) %>%
  group_by(STATE_ABBR_2010SVI) %>%
  summarise(model = list(lm(upward_mobility_rate_2010 ~ POP2010)))


library(dplyr)

#model first - before nesting

upward_models <- merged %>%
  filter(!is.na(STATE_ABBR_2010SVI),
         !is.na(upward_mobility_rate_2010), 
         !is.na(POP2010)) %>%
    group_by(STATE_ABBR_2010SVI) %>%
    summarise(model = list(lm(upward_mobility_rate_2010 ~ POP2010)))

merged <- nest_by(state_group)

merged <- merged_data

az <- merged_data[merged_data$STATE_NAME_2010SVI == "Arizona", ]
ca <- merged_data[merged_data$STATE_NAME_2010SVI == "California", ]

dim(merged_data)

print(nrow(az))

print(nrow(ca))

t.test(x = az$upward_mobility_rate_2010, y = ca$upward_mobility_rate_2010)

aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)

mobility_rate_az_aov <- aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)

summary(object = mobility_rate_az_aov)

sink(file = "output/az_mobility_anova.txt")
summary(object = mobility_rate_az_aov)
sink()

plot(x = merged_data$upward_mobility_rate_2010, y = merged_data$M_TOTPOP_2010SVI)

#Multiple group comparisons - ANOVA analysis of variance
#allows multiple groups to be compared at once

aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)

mobility_rate_az_aov <- aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)

summary(object = mobility_rate_az_aov)

sink(file = "output/az_mobility_anova.txt")
summary(object = mobility_rate_az_aov)
sink()


#Linear Regression
summary(merged_data[, 1:10], 6)

plot(x = merged_data$upward_mobility_rate_2010, y = merged_data$M_TOTPOP_2010SVI)

merged$logpop <- log10(merged$M_TOTPOP_2010SVI)

plot(x = merged$upward_mobility_rate_2010, 
     y = merged$logpop, 
     xlab = "Upward Mobility", 
     ylab = "log10(Population)")

mobility_v_pop <- lm(upward_mobility_rate_2010 ~ logpop, data = merged)

summary(mobility_v_pop)

sink(file = "output/mobility-pop-regression.txt")
summary(mobility_v_pop)
sink()

merged$az <- ifelse(merged$STATE_NAME_2010SVI == "Arizona", 1, 0)
merged$ca <- ifelse(merged$STATE_NAME_2010SVI == "California", 1, 0)

mobility_v_pop_state <- lm(formula = upward_mobility_rate_2010 ~ logpop + az
                           , data = merged)
summary(mobility_v_pop_state)

sink(file = "output/mobility-pop-state-regression.txt")
summary(mobility_v_pop)
sink()

#Student Activity- variable exploration, 
#Pick a variable from the environment section and make a box plot to explore it. 

