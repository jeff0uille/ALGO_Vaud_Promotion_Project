# Load the CSV file into a data frame
data <- read.csv(here("data", "TMS_dataset_Vaud_20240202_15.03Extract.csv"), sep = ";")

### Visualize the average spending per category (without a package tour)

# Assuming 'data' is your dataframe
library(ggplot2)
library(dplyr)
library(plotly)

# Create an aggregated data frame of spendings with new category names
spending_data <- data.frame(
  Spending_Type = c("Overnight stay", "Transports", "Food & Drink", "Souvenirs", "Other shopping", "Other expenses"),
  Amount = c(mean(data$F81_01, na.rm = TRUE), 
             mean(data$F81_02, na.rm = TRUE),
             mean(data$F81_03, na.rm = TRUE),
             mean(data$F81_04, na.rm = TRUE),
             mean(data$F81_05, na.rm = TRUE),
             mean(data$F81_06, na.rm = TRUE))
)

# Plotting the bar chart with the new category names
ggplot(spending_data, aes(x = Spending_Type, y = Amount, fill = Spending_Type)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Average Spending by Category (no package tour)", x = "Category", y = "Average Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability






### Visualize the average spending per category (with a package tour)

# Create an aggregated data frame of spendings with new category names
spending_data_package <- data.frame(
  Spending_Type = c("Package", "Overnight stay", "Transports", "Food & Drink", "Souvenirs", "Other shopping", "Other expenses"),
  Amount = c(mean(data$F82_01, na.rm = TRUE), 
             mean(data$F82_02, na.rm = TRUE),
             mean(data$F82_03, na.rm = TRUE),
             mean(data$F82_04, na.rm = TRUE),
             mean(data$F82_05, na.rm = TRUE),
             mean(data$F82_06, na.rm = TRUE),
             mean(data$F82_07, na.rm = TRUE))
)

# Plotting the bar chart with the new category names
ggplot(spending_data_package, aes(x = Spending_Type, y = Amount, fill = Spending_Type)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Average Spending by Category", x = "Category", y = "Average Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability








### How many ppl with the package are Swiss?

# Replace these with the actual values or conditions from your dataset
data_filtered <- data %>%
  filter(`F30.ENG` == "package_indicator", `F05_02.ENG` == "Switzerland_indicator")

# Count the number of people from Switzerland who took packages
swiss_package_counts <- data_filtered %>%
  count(`F05_02.ENG`)

# Visualize the result
ggplot(swiss_package_counts, aes(x = `F05_02 ENG`, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of People from Switzerland Who Took Packages",
       x = "Country", y = "Count") +
  theme_minimal()






### Visualize repartition of currencies

# Calculate the count and percentage of each currency
currency_counts <- data %>%
  count(Currency) %>%
  mutate(Percentage = n / sum(n) * 100)

# Create a plot to see the repartition of the currencies with percentages
ggplot(currency_counts, aes(x = Currency, y = n, fill = Currency)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(title = "Repartition of Currencies Within the Dataset",
       x = "Currency",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for readability








### Spending by Nationality (overall contribution)

# Renaming the F05_01.ENG column to Continent and filtering the data
filtered_data <- data %>%
  rename(Continent = F05_01.ENG) %>%
  filter(F81_Total <= 100000)

# Plotting the filtered data with continents colored and legend below the plot
ggplot(filtered_data, aes(x = F05_02.ENG, y = F81_Total, fill = Continent)) +
  geom_boxplot() +
  labs(title = "Spending by Nationality", x = "Nationality", y = "Spending") +
  scale_fill_brewer(palette = "Set3") + # Adds a color palette for the continents
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") # Moves the legend to the bottom