# Load required libraries
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(RColorBrewer)

# Load and inspect the dataset
train_data <- read.csv("train.csv")
train <- train_data

# Initial overview of the dataset
view(train)      # View data in table format
glimpse(train)   # View structure of the data
summary(train)   # Summary statistics of the dataset
sapply(train, function(x) sum(is.na(x)))  # Count missing values in each column

# Remove the Id column as it is not useful for analysis
train$Id <- NULL

# Convert quality columns to categorical variables
train$OverallQual <- as.factor(train$OverallQual)
train$OverallCond <- as.factor(train$OverallCond)

# Calculate and visualize central tendency measures of SalePrice
sale_price_mode <- names(table(train$SalePrice)[table(train$SalePrice) == max(table(train$SalePrice))])
sale_price_summary <- data.frame(
  measure = c("Mean", "Median", "Mode"),
  value = c(mean(train$SalePrice), median(train$SalePrice), as.numeric(sale_price_mode))
)

ggplot(sale_price_summary, aes(x = measure, y = value)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "blue") +
  ggtitle("Central Tendency Measures of SalePrice")

# Remove outliers from SalePrice to plot a cleaner distribution
Q1 <- quantile(train$SalePrice, 0.25)
Q3 <- quantile(train$SalePrice, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
train_filtered <- train %>% filter(SalePrice >= lower_bound & SalePrice <= upper_bound)

# Plot SalePrice distribution (without outliers)
ggplot(train_filtered, aes(x = SalePrice)) +
  geom_histogram(binwidth = 10000, fill = "#69b3a2", color = "#e9ecef") +
  scale_x_continuous(labels = scales::comma) +
  labs(x = "Sale Price", y = "Frequency", title = "Distribution of House Sale Prices") +
  theme_minimal()

# Correlation analysis between numerical variables and SalePrice
numeric_features <- train %>% select_if(is.numeric)
glimpse(numeric_features)

# Display correlation matrix with Spearman correlation
GGally::ggcorr(
  numeric_features, method = c("pairwise", "spearman"),  
  label = TRUE, hjust = 1, label_size = 2, layout.exp = 10, size = 3
)

# Create a correlation table for numeric variables
cor_matrix <- cor(numeric_features, method = "spearman", use = "pairwise")
cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
cor_df <- as.data.frame(as.table(cor_matrix)) %>%
  filter(!is.na(Freq) & Var1 != Var2) %>%
  arrange(-abs(Freq))

# Visualize relation between YearBuilt and SalePrice by Decades
train$Decade <- cut(train$YearBuilt, breaks = seq(1870, 2010, by = 10), 
                    labels = paste0(seq(1870, 2000, by = 10), "-", seq(1880, 2010, by = 10)))

ggplot(train, aes(x = Decade, y = SalePrice)) + 
  geom_bar(stat = "summary", fun = mean, fill = "#0072B2") +
  labs(title = "Average SalePrice by Decade of Construction", x = "Decade", y = "Average SalePrice") +
  theme_bw()

# Plot SalePrice vs Garage Capacity (filtered data)
ggplot(train_filtered, aes(x = SalePrice, fill = as.factor(GarageCars))) +
  geom_density(alpha = 0.4) +
  xlab("SalePrice") +
  ylab("Density") +
  ggtitle("Density of SalePrice by Garage Capacity")

# Scatter plot for SalePrice vs Living Area (GrLivArea)
mean_grlivarea <- mean(train$GrLivArea)
sd_grlivarea <- sd(train$GrLivArea)
train_filtered_area <- train[train$GrLivArea < mean_grlivarea + 3 * sd_grlivarea, ]

ggplot(train_filtered_area, aes(x = GrLivArea, y = SalePrice)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("Ground Living Area (GrLivArea)") + ylab("SalePrice") +
  ggtitle("Relationship between SalePrice and GrLivArea") +
  scale_y_continuous(limits = c(0, 800000))

# Additional analyses on other relationships
# Relationships such as Overall Quality (OverallQual) and SalePrice

anova_quality <- aov(SalePrice ~ OverallQual, data = train)
summary(anova_quality)

ggplot(train, aes(x = OverallQual, y = SalePrice)) +
  geom_boxplot(fill = "lightblue") +
  ggtitle("SalePrice Distribution by Overall Quality") +
  xlab("OverallQual") +
  ylab("SalePrice")

# Additional analysis on ExterQual, Neighborhood, and Basement Quality (BsmtQual)


