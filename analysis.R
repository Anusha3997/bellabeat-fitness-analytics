# =========================================
# Bellabeat Fitbit Case Study
# Author: Anusha Nagula
# =========================================

# ---------- Libraries ----------
library(tidyverse)
library(lubridate)
library(here)

# ---------- Paths ----------
data_path <- here("dataset")

# sanity check
print(getwd())
print(list.files(data_path))


# =========================================
# Load Data (SAFE + SIMPLE)
# =========================================
# IMPORTANT:
# 1. Always read CSV normally
# 2. Convert datetime AFTER loading
# 3. This avoids parsing crashes
# =========================================

daily       <- read_csv(file.path(data_path, "dailyActivity_merged.csv"), show_col_types = FALSE) # nolint
steps       <- read_csv(file.path(data_path, "hourlySteps_merged.csv"), show_col_types = FALSE) # nolint
calories    <- read_csv(file.path(data_path, "hourlyCalories_merged.csv"), show_col_types = FALSE) # nolint
intensities <- read_csv(file.path(data_path, "hourlyIntensities_merged.csv"), show_col_types = FALSE) # nolint
sleep       <- read_csv(file.path(data_path, "minuteSleep_merged.csv"), show_col_types = FALSE) # nolint
weight      <- read_csv(file.path(data_path, "weightLogInfo_merged.csv"), show_col_types = FALSE) # nolint


# =========================================
# Convert datetime columns (robust)
# =========================================

steps$ActivityHour    <- mdy_hms(steps$ActivityHour, quiet = TRUE)
calories$ActivityHour <- mdy_hms(calories$ActivityHour, quiet = TRUE)
intensities$ActivityHour <- mdy_hms(intensities$ActivityHour, quiet = TRUE)


# =========================================
# 1. Average Steps by Hour
# =========================================

steps_by_hour <- steps %>%
  mutate(hour = hour(ActivityHour)) %>%
  group_by(hour) %>%
  summarise(total_steps = sum(StepTotal, na.rm = TRUE), .groups = "drop")

print(ggplot(steps_by_hour, aes(hour, total_steps)) +
  geom_col(fill = "#3498DB") +
  labs(title = "Average Steps by Hour",
       x = "Hour of Day",
       y = "Total Steps")
)

# =========================================
# 2. Calories Burned by Hour
# =========================================

calories_by_hour <- calories %>%
  mutate(hour = hour(ActivityHour)) %>%
  group_by(hour) %>%
  summarise(total_calories = sum(Calories, na.rm = TRUE), .groups = "drop")

print(ggplot(calories_by_hour, aes(hour, total_calories)) +
  geom_col(fill = "#1F618D") +
  labs(title = "Calories Burned by Hour",
       x = "Hour of Day",
       y = "Calories")
)

# =========================================
# 3. BMI vs Activity
# =========================================

bmi_activity <- inner_join(
  daily,
  weight,
  by = c("Id", "ActivityDate" = "Date")
)

print(ggplot(bmi_activity, aes(BMI, TotalSteps)) +
  geom_point(alpha = 0.6, color = "#2E86C1") +
  geom_smooth(method = "lm", se = FALSE, color = "gray40") +
  labs(title = "BMI vs Total Steps",
       x = "BMI",
       y = "Total Steps"))