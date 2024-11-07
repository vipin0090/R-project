# Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Load the dataset (replace with your dataset file path if needed)
data <- read.csv("Superstore.csv")

# Data preprocessing (adjust as needed based on your dataset)
data$Order.Date <- as.Date(data$Order.Date, format = "%m/%d/%Y")
data$Year <- year(data$Order.Date)
data$Month <- month(data$Order.Date, label = TRUE)
data$Category <- factor(data$Category)

# Visualization 1: Monthly Sales Trend
ggplot(data, aes(x = Month, y = Sales, group = Year, color = factor(Year))) +
  geom_line(size = 1) +
  labs(title = "Monthly Sales Trend by Year", x = "Month", y = "Total Sales") +
  theme_minimal()

# Visualization 2: Category-wise Sales Distribution
ggplot(data, aes(x = Category, y = Sales, fill = Category)) +
  geom_boxplot() +
  labs(title = "Sales Distribution by Product Category", x = "Category", y = "Sales") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()

# Visualization 3: Top 10 Cities by Sales
top_cities <- data %>%
  group_by(City) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  arrange(desc(Total_Sales)) %>%
  top_n(10)

ggplot(top_cities, aes(x = reorder(City, -Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Cities by Total Sales", x = "City", y = "Total Sales") +
  theme_minimal() +
  coord_flip()

# Visualization 4: Sales by Customer Segment
ggplot(data, aes(x = Segment, y = Sales, fill = Segment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sales by Customer Segment", x = "Customer Segment", y = "Sales") +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()

# Visualization 5: Profit vs Sales Scatter Plot
ggplot(data, aes(x = Sales, y = Profit, color = Category)) +
  geom_point(alpha = 0.6) +
  labs(title = "Profit vs Sales by Category", x = "Sales", y = "Profit") +
  theme_minimal() +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::dollar_format())
