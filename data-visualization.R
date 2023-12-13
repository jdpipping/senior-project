# load libraries
library(corrplot)
library(tidyverse)

# load dataset
data <- read_csv('final-data.csv')

# motivation for pls ####

# correlation matrix
correlation_matrix <- cor(data |> select(-fips, -state, -county, -avg_days_mentally_unhealthy))

# correlation plot
corrplot(correlation_matrix, method = "color", tl.pos = 'n')

# post-modeling visualization ####

# household income / mental health
data |> 
  ggplot(aes(x = median_household_income,
             y = avg_days_mentally_unhealthy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Poor Mental Health Days vs Median Household Income',
       y = 'Average Days Mentally Unhealthy (Last 30)',
       x = 'Median Household Income ($)') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# physical / mental health
data |> 
  ggplot(aes(x = avg_days_physically_unhealthy,
             y = avg_days_mentally_unhealthy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Poor Mental Health vs Poor Physical Health',
       y = 'Average Days Mentally Unhealthy (Last 30)',
       x = 'Average Days Physically Unhealthy (Last 30)') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# sleep / mental health
data |> 
  ggplot(aes(x = perc_insufficient_sleep,
             y = avg_days_mentally_unhealthy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Poor Mental Health Days vs Insufficient Sleep',
       y = 'Average Days Mentally Unhealthy (Last 30)',
       x = '% of Adults Averaging <7 Hours of Sleep') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# smoking / mental health
data |> 
  ggplot(aes(x = perc_adults_smoking,
             y = avg_days_mentally_unhealthy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Poor Mental Health Days vs Adults Smoking',
       y = 'Average Days Mentally Unhealthy (Last 30)',
       x = '% of Adults Who Smoke') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# food insecurity / mental health

data |> 
  ggplot(aes(x = perc_food_insecure,
             y = avg_days_mentally_unhealthy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Poor Mental Health Days vs Food Insecurity',
       y = 'Average Days Mentally Unhealthy (Last 30)',
       x = '% of Population Lacking Access to Food') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# physical inactivity / mental health

data |> 
  ggplot(aes(x = perc_physically_inactive,
             y = avg_days_mentally_unhealthy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Poor Mental Health Days vs Physical Inactivity',
       y = 'Average Days Mentally Unhealthy (Last 30)',
       x = '% of Adults w/o Physical Activity') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# child care cost / mental health

data |> 
  ggplot(aes(x = perc_income_required_child_care,
             y = avg_days_mentally_unhealthy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Poor Mental Health Days vs Child Care Burden',
       y = 'Average Days Mentally Unhealthy (Last 30)',
       x = '% of Income Required for Child Care') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# social associations / mental health

data |> 
  ggplot(aes(x = social_association_rate,
             y = avg_days_mentally_unhealthy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Poor Mental Health Days vs Social Association Rate',
       y = 'Average Days Mentally Unhealthy (Last 30)',
       x = 'Social Associations (per 10,000)') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# broadband access / mental health

data |> 
  ggplot(aes(x = perc_households_broadband_access,
             y = avg_days_mentally_unhealthy)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Poor Mental Health Days vs Broadband Access',
       y = 'Average Days Mentally Unhealthy (Last 30)',
       x = '% of Households w/ Broadband Access') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))