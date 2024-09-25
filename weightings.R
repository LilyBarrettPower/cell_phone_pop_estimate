# Load necessary libraries
library(readr)      # For reading CSV and gzipped files
library(arrow)      # For reading parquet files (install with install.packages("arrow"))
library(dplyr)      # For data manipulation
library(lubridate)
library(ggplot2)
library(tidyverse)

# Load the telecommunications data
sp_data <- read_csv("sp_data.csv.gz")
vf_data <- read_parquet("vf_data.parquet")


# Load the population estimates
pop_estimates <- read_csv("subnational_pop_ests.csv")

# Load the SA2 geographical data
#sa2_codes <- read_csv("sa2_2023.csv")
#sa2_ta_concord <- read_csv("sa2_ta_concord_2023.csv")

head(vf_data)
head(sp_data)
head(pop_estimates)

######################################################################
# Combine sp and vf data based on sa2 area code 

# First, rename columns to have the same name for merging
vf_data <- vf_data %>% rename(sa2 = area)  # Rename 'area' to 'sa2' to match sp_data

# Convert 'sa2' in vf_data to numeric to match sp_data
vf_data <- vf_data %>%
  mutate(sa2 = as.numeric(sa2))

# Rename Spark timestamp for easier merging
sp_data <- sp_data %>%
  rename(dt = ts)  

# Merge by SA2 and time
merged_data <- vf_data %>%
  left_join(sp_data, by = c("sa2", "dt"))

# Step 2: Sum the counts
merged_data <- merged_data %>%
  mutate(total_count = devices + cnt) %>%
  select(sa2, dt, total_count)

head(merged_data)

######################################################################
# Clean population data 
# Keep only rows where AGE_POPES_SUB_006 is TOTALALLAGES and filter out non-SA2 areas like NZTA
cleaned_pop_estimates <- pop_estimates %>%
  filter(AGE_POPES_SUB_006 == "TOTALALLAGES" & AREA_POPES_SUB_006 != "NZTA") %>%
  mutate(SA2_code = as.numeric(AREA_POPES_SUB_006))

# Sum the population for each SA2 code to get total population
total_population_by_sa2 <- cleaned_pop_estimates %>%
  group_by(SA2_code) %>%
  summarise(total_population = sum(OBS_VALUE, na.rm = TRUE))

# View the total population by SA2 code
head(total_population_by_sa2)

######################################################################
# Merge the population data with the vf and sp data 

# Merge with the total population by SA2 code
merged_data_with_population <- merged_data %>%
  left_join(total_population_by_sa2, by = c("sa2" = "SA2_code"))

# Calculate weighting factor
merged_data_with_population <- merged_data_with_population %>%
  mutate(weight = total_population / total_count)

# Estimate the population using the weighting factor
merged_data_with_population <- merged_data_with_population %>%
  mutate(estimated_population = total_count * weight)

# View the result
head(merged_data_with_population)


######################################################################

# View the estimated population for each sa2 code:

# Group by SA2 code and summarize the estimated population
estimated_population_by_sa2 <- merged_data_with_population %>%
  group_by(sa2) %>%
  summarise(average_estimated_population = mean(estimated_population, na.rm = TRUE),
            total_estimated_population = sum(estimated_population, na.rm = TRUE))

# View the summarized estimated population by SA2
print(estimated_population_by_sa2)


#Use the average_estimated_population if you're interested in the typical population 
####at any given point in time for each SA2.

#Use the total_estimated_population if you want the overall estimated population 
###for each SA2 area across the entire dataset period.


######################################################################

# Compare the average estimated population to the actual population for each sa2 zone

# Merge the estimated population data with the actual population data
comparison_data <- estimated_population_by_sa2 %>%
  left_join(total_population_by_sa2, by = c("sa2" = "SA2_code"))

# Calculate the difference between the actual population and the average estimated population
comparison_data <- comparison_data %>%
  mutate(
    difference = total_population - average_estimated_population,
    percentage_difference = (difference / total_population) * 100
  )

# View the comparison
print(comparison_data)

######################################################################
# View the comparision between estimated population and actual population 

# Create a comparison plot between estimated and actual population
population_comparison_plot <- ggplot(comparison_data, aes(x = total_population, y = average_estimated_population)) +
  geom_point(color = "blue") +  # Plot points for comparison
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Line for perfect match
  labs(title = "Comparison of Estimated vs Actual Population",
       x = "Actual Population",
       y = "Estimated Population") +
  theme_minimal()

# Display the plot
print(population_comparison_plot)
