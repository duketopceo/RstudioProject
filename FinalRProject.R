# University Rankings Analysis Project

# Load required packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

# Data Ingestion
# Use your specific file path or just the filename if the CSV is in your project root.
data <- read_csv("qs-world-rankings-2025.csv")

# Data Cleaning
# Check for missing values and column names
summary(data)
colnames(data)

# Handle missing values
data_clean <- data %>%
  filter(!is.na(`QS Overall Score`)) %>%
  mutate(
    `Academic Reputation` = ifelse(is.na(`Academic Reputation`), median(`Academic Reputation`, na.rm = TRUE), `Academic Reputation`),
    `Employer Reputation` = ifelse(is.na(`Employer Reputation`), median(`Employer Reputation`, na.rm = TRUE), `Employer Reputation`)
  )

# Remove duplicates
data_clean <- data_clean %>% distinct()

# Convert rank and score columns to numeric
data_clean <- data_clean %>%
  mutate(
    rank = as.numeric(gsub("[^0-9]", "", `2025 Rank`)),
    `QS Overall Score` = as.numeric(`QS Overall Score`)
  )

# Data Manipulation using dplyr
# 1. Filtering - Top 100 universities
top_100 <- data_clean %>%
  filter(rank <= 100)

# 2. Selecting relevant columns
selected_data <- data_clean %>%
  select(`Institution Name`, Location, rank, `QS Overall Score`, `Academic Reputation`, `Employer Reputation`)

# 3. Mutating - Create performance categories
data_analysis <- selected_data %>%
  mutate(
    performance_tier = case_when(
      rank <= 50 ~ "Top Tier",
      rank <= 100 ~ "High Tier", 
      rank <= 200 ~ "Mid Tier",
      TRUE ~ "Lower Tier"
    )
  )

# 4. Grouped Summaries
country_summary <- data_analysis %>%
  group_by(Location) %>%
  summarise(
    avg_score = mean(`QS Overall Score`, na.rm = TRUE),
    count = n(),
    max_rank = min(rank, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_score))

# 5. Arranging data
arranged_data <- data_analysis %>%
  arrange(desc(`QS Overall Score`))

# Statistical Analysis
# Descriptive Statistics
mean_score <- mean(data_analysis$`QS Overall Score`, na.rm = TRUE)
median_score <- median(data_analysis$`QS Overall Score`, na.rm = TRUE)
sd_score <- sd(data_analysis$`QS Overall Score`, na.rm = TRUE)
min_score <- min(data_analysis$`QS Overall Score`, na.rm = TRUE)
max_score <- max(data_analysis$`QS Overall Score`, na.rm = TRUE)

# Print descriptive statistics
cat("Mean Overall Score:", mean_score, "\n")
cat("Median Overall Score:", median_score, "\n") 
cat("Standard Deviation:", sd_score, "\n")
cat("Minimum Score:", min_score, "\n")
cat("Maximum Score:", max_score, "\n")

# Hypothesis Testing - t-test
# Compare academic reputation between top 50 and next 50 universities
top_50_academic <- data_analysis %>% filter(rank <= 50) %>% pull(`Academic Reputation`)
next_50_academic <- data_analysis %>% filter(rank > 50 & rank <= 100) %>% pull(`Academic Reputation`)

t_test_result <- t.test(top_50_academic, next_50_academic)
print(t_test_result)

# Linear Regression
# Predict overall score based on academic and employer reputation
lm_model <- lm(`QS Overall Score` ~ `Academic Reputation` + `Employer Reputation`, data = data_analysis)
summary(lm_model)

# Visualizations using ggplot2
# Visualization 1: Distribution of Overall Scores
plot1 <- ggplot(data_analysis, aes(x = `QS Overall Score`)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of University Overall Scores",
       x = "Overall Score",
       y = "Frequency") +
  theme_minimal()

ggsave("score_distribution.png", plot1, width = 10, height = 6, dpi = 300)

# Visualization 2: Top 20 Countries by Average Score
top_countries <- country_summary %>% head(20)

plot2 <- ggplot(top_countries, aes(x = reorder(Location, avg_score), y = avg_score)) +
  geom_col(fill = "coral", alpha = 0.8) +
  coord_flip() +
  labs(title = "Top 20 Countries by Average University Score",
       x = "Country",
       y = "Average Overall Score") +
  theme_minimal()

ggsave("top_countries.png", plot2, width = 10, height = 8, dpi = 300)

# Visualization 3: Scatter plot of Academic vs Employer Reputation
plot3 <- ggplot(data_analysis, aes(x = `Academic Reputation`, y = `Employer Reputation`, color = performance_tier)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Academic Reputation vs Employer Reputation by Performance Tier",
       x = "Academic Reputation Score",
       y = "Employer Reputation Score",
       color = "Performance Tier") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("reputation_scatter.png", plot3, width = 10, height = 6, dpi = 300)

# Export cleaned data
write_csv(data_analysis, "cleaned_university_data.csv")

