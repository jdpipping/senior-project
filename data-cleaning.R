# load tidyverse
library(tidyverse)

# read in data
data <- read_csv('2023-county-data.csv')
additional_data <- read_csv('2023-additional-data.csv')

# create county-level dataset
county_data <- data |> 
  # join datasets
  left_join(additional_data, by = c('FIPS')) |> 
  # make fips code numeric
  mutate(FIPS = as.numeric(FIPS)) |> 
  # remove state-level data
  filter(FIPS %% 1000 != 0)

# create final dataset
final_data <- county_data |> 
  # select desired columns
  reframe(
    # omit columns with high missingness
    fips = FIPS,
    state = State.x,
    county = County.x,
    # ypll_rate = `Years of Potential Life Lost Rate`,
    perc_fair_or_poor_health = `% Fair or Poor Health`,
    avg_days_physically_unhealthy = `Average Number of Physically Unhealthy Days`,
    avg_days_mentally_unhealthy = `Average Number of Mentally Unhealthy Days`,
    perc_low_birthweight = `% Low Birthweight`,
    perc_adults_smoking = `% Adults Reporting Currently Smoking`,
    perc_adults_obseity = `% Adults with Obesity`,
    food_environment_index = `Food Environment Index`,
    perc_physically_inactive = `% Physically Inactive`,
    perc_with_exercise_opportunities = `% With Access to Exercise Opportunities`,
    perc_excessive_drinking = `% Excessive Drinking`,
    perc_driving_deaths_alcohol = `% Driving Deaths with Alcohol Involvement`,
    chlamydia_rate = `Chlamydia Rate`,
    # teen_birth_rate = `Teen Birth Rate`,
    perc_uninsured = `% Uninsured`,
    primary_care_physicians_ratio = 1 / as.numeric(substr(`Primary Care Physicians Ratio`, 1, nchar(`Primary Care Physicians Ratio`) - 2)),
    dentist_ratio = 1 / as.numeric(substr(`Dentist Ratio`, 1, nchar(`Dentist Ratio`) - 2)),
    mental_health_provider_ratio = 1 / as.numeric(substr(`Mental Health Provider Ratio`, 1, nchar(`Mental Health Provider Ratio`) - 2)),
    preventable_hospitalization_rate = `Preventable Hospitalization Rate`,
    perc_annual_mammagram = `% with Annual Mammogram`,
    perc_vaccinated = `% Vaccinated`,
    perc_completed_high_school = `% Completed High School`,
    perc_some_college = `% Some College`,
    perc_unemployed = `% Unemployed`,
    perc_children_in_poverty = `% Children in Poverty`,
    income_ratio_20_80 = 1 / `Income Ratio`,
    perc_children_single_parent = `% Children in Single-Parent Households`,
    social_association_rate = `Social Association Rate`,
    injury_death_rate = `Injury Death Rate`,
    avg_daily_pm = `Average Daily PM2.5`,
    water_violation = as.numeric(`Presence of Water Violation` == 'Yes'),
    perc_severe_housing_problems = `% Severe Housing Problems`,
    severe_housing_cost_burden = `Severe Housing Cost Burden`,
    overcrowding = `Overcrowding`,
    inadequate_facilities = `Inadequate Facilities`,
    perc_drive_alone_to_work = `% Drive Alone to Work`,
    perc_long_commute_alone = `% Long Commute - Drives Alone`,
    life_expectancy = `Life Expectancy`,
    # age_adjusted_death_rate = `Age-Adjusted Death Rate`,
    # child_mortality_rate = `Child Mortality Rate`,
    # infant_mortality_rate = `Infant Mortality Rate`,
    # outcome: perc_frequent_physical_distress = `% Frequent Physical Distress`,
    # outcome: perc_frequent_mental_distress = `% Frequent Mental Distress`,
    perc_adults_diabetes = `% Adults with Diabetes`,
    # hiv_prevalence_rate = `HIV Prevalence Rate`,
    perc_food_insecure = `% Food Insecure`,
    perc_limited_access_healthy_foods = `% Limited Access to Healthy Foods`,
    # drug_overdose_mortality_rate = `Drug Overdose Mortality Rate`,
    perc_insufficient_sleep = `% Insufficient Sleep`,
    perc_uninsured_adults = `% Uninsured Adults`,
    perc_uninsured_children = `% Uninsured Children`,
    other_primary_care_provider_ratio = 1 / as.numeric(substr(`Other Primary Care Provider Ratio`, 1, nchar(`Other Primary Care Provider Ratio`) - 2)),
    # high_school_graduation_rate = `High School Graduation Rate`,
    # perc_disconnected_youth = `% Disconnected Youth`,
    # avg_reading_performance = `Average Grade Performance...131`,
    # avg_math_performance = `Average Grade Performance...136`,
    # school_segregation_index = `Segregation Index...141`,
    school_funding_accuracy = `School Funding Adequacy`,
    gender_pay_gap = `Gender Pay Gap`,
    median_household_income = `Median Household Income`,
    # perc_enrolled_free_reduced_lunch = `% Enrolled in Free or Reduced Lunch`,
    # residential_segregation_index = `Segregation Index...168`,
    perc_income_required_child_care = `% Household Income Required for Child Care Expenses`,
    child_care_center_rate = `Child Care Centers per 1,000 Children`,
    # homicide_rate = `Homicide Rate`,
    # age_adjusted_suicide_rate = `Suicide Rate (Age-Adjusted)`,
    # firearm_fatality_rate = `Firearm Fatalities Rate`,
    # motor_vehicle_mortality_rate = `Motor Vehicle Mortality Rate`,
    # juvenile_arrest_rate = `Juvenile Arrest Rate`,
    perc_voter_turnout = `% Voter Turnout`,
    perc_census_participation = `% Census Participation`,
    traffic_volume = `Traffic Volume`,
    perc_homeowners = `% Homeowners`,
    perc_households_severe_cost_burden = `% Households with Severe Cost Burden`,
    perc_households_broadband_access = `% Households with Broadband Access`,
    population = `Population`,
    perc_under_18 = `% Less than 18 Years of Age`,
    perc_65_over = `% 65 and Over`,
    perc_black = `% Black`,
    perc_ai_an = `% American Indian or Alaska Native`,
    perc_asian = `% Asian`,
    perc_nh_opi = `% Native Hawaiian or Other Pacific Islander`,
    perc_hispanic = `% Hispanic`,
    perc_non_hispanic_white = `% Non-Hispanic White`,
    perc_not_proficient_english = `% Not Proficient in English`,
    perc_female = `% Female`,
    perc_rural = `% Rural`
  ) |> 
  # omit rows with missing data
  na.omit()

# write final dataset
write_csv(final_data, 'final-data.csv')
