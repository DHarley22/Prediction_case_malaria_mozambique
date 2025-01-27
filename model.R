
# load libraries ----------------------------------------------------------



library(tidyverse)
library(sf)
library(sp)
library(lubridate)
library(naniar)
library(viridisLite)
library(viridis)
library(dplyr)
library(ggplot2)
library(dplyr)
library(corrplot)



# load dataset ------------------------------------------------------------

df <- readRDS("Mozambique data_tutorial_STUDENTS_revised.rds")
moz_shapefile_sf <- st_as_sf(df$moz_shapefile)

analysis_data <- df$analysis_data %>%
  ungroup()


new_data <- df$analysis_data %>%
  filter(district == "angonia") %>%
  mutate(date = make_date(year, month))  # Create a proper date column


# Reshape the data for easier plotting
plot_data <- new_data %>%
  select(date, malaria_cases_u5, diarr_cases_u5) %>%
  tidyr::pivot_longer(
    cols = c(malaria_cases_u5, diarr_cases_u5),
    names_to = "case_type",
    values_to = "cases"
  )

# Plot malaria and diarrhea cases in the same plot
ggplot(plot_data, aes(x = date, y = cases, color = case_type)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Malaria and Diarrhea Cases in 	angonia",
    x = "Date",
    y = "Number of Cases (Under 5 Years)",
    color = "Case Type"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Preprocessing data ------------------------------------------------------

# List of variables to keep based on relevance
variables_to_keep <- c(
  "district", "year", "month", "malaria_cases_u5", "diarr_cases_u5", 
  "tmin", "tmax", "precipitation", "RH", "ndvi", "prop_poor", "prop_Rural", 
  "prop_drinking_TreatedWater", "prop_Has_sleeping_mosquito_net", 
  "prop_dwelling_sprayed_last_12_Months", "prop_uneducated", "number_of_doctors"
)

# Filter the dataset to keep only relevant variables
analysis_data_cleaned <- analysis_data %>%
  select(all_of(variables_to_keep))

# Check for missing values
miss_var_summary(analysis_data_cleaned)

# Split the dataset into training, test, and evaluation sets --------------

# Training data: All data before March 2018 (remove missing target variables)
training_data <- analysis_data_cleaned %>%
  filter(year < 2018 | (year == 2018 & month < 3)) %>%
  filter(!is.na(malaria_cases_u5) & !is.na(diarr_cases_u5))  # Remove missing target values

# Test data: Data for March-June 2018 (keep rows even if target variables are missing)
test_data <- analysis_data_cleaned %>%
  filter(year == 2018 & month >= 3 & month <= 6)

# Evaluation data: Data for July-December 2018 (remove missing target variables)
evaluation_data <- analysis_data_cleaned %>%
  filter(year == 2018 & month >= 7) %>%
  filter(!is.na(malaria_cases_u5) & !is.na(diarr_cases_u5))  # Remove missing target values

# Verify the split
cat("Training data dimensions:", dim(training_data), "\n")
cat("Test data dimensions:", dim(test_data), "\n")
cat("Evaluation data dimensions:", dim(evaluation_data), "\n")

# Save the datasets (optional)
#saveRDS(training_data, "training_data.rds")
#saveRDS(test_data, "test_data.rds")
#saveRDS(evaluation_data, "evaluation_data.rds")



# Exploratory Data Analysis (EDA) -----------------------------------------



# Summary statistics for training data
summary(training_data)

# Correlation matrix for numeric variables in training data
numeric_vars <- training_data %>%
  select(where(is.numeric)) %>%
  select(-year, -month)  # Exclude year and month for correlation analysis

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

# Visualize correlations
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

# Temporal trends for all districts in training data
all_districts_plot_data <- training_data %>%
  mutate(date = make_date(year, month)) %>%
  select(date, district, malaria_cases_u5, diarr_cases_u5) %>%
  tidyr::pivot_longer(
    cols = c(malaria_cases_u5, diarr_cases_u5),
    names_to = "case_type",
    values_to = "cases"
  )

# Plot temporal trends for all districts
ggplot(all_districts_plot_data, aes(x = date, y = cases, color = district)) +
  geom_line(size = 0.5, alpha = 0.7) +
  facet_wrap(~case_type, scales = "free_y") +
  labs(
    title = "Temporal Trends of Malaria and Diarrhea Cases Across Districts (Training Data)",
    x = "Date",
    y = "Number of Cases (Under 5 Years)",
    color = "District"
  ) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 months") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

