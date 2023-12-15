#Loading CSV Files
sales <- read.csv("C:/Applied Stat Report/statsfinal.csv", header=FALSE)
View(sales)
dim(sales)
head(sales)
summary(sales)
str(sales)
missing_value <- is.na(sales)
missing_value
column_missing <- colSums(missing_value)
column_missing
duplicates <- sales[duplicated(sales), ]
duplicates
t(summary(sales))
# Data Pre-processing 
#Converting date into single format 
sales$V2 <- as.Date(gsub("/", "-", sales$V2), format = "%d-%m-%Y")

# Convert the date column to POSIXlt
sales$V2 <- as.POSIXlt(sales$V2, format="%Y-%m-%d")

# Extract year, month, day, and day of the week
sales$Year <- format(sales$V2, "%Y")
sales$Month <- format(sales$V2, "%m")
sales$Month_Name <- format(sales$V2, "%B")
sales$Day <- format(sales$V2, "%d")
sales$Day_Of_Week <- weekdays(sales$V2)

# checking null values
total_null_value <- sum(is.na(sales))
total_null_value
null_values_per_column <- colSums(is.na(sales))
null_values_per_column
# Remove rows with NA values
sales <- na.omit(sales)


#Converting V3 to V10 into numerric
sales[, c("V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10")] <-
  lapply(sales[, c("V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10")], as.numeric)

head(sales)
t(summary(sales))
str(sales)

# Drop column V1 
sales <- sales[,-1]

library(ggplot2)

# Total unit sales Product 1, Product 2, Product 3, Product 4
total_unit <- colSums(sales[, c("V3", "V4", "V5", "V6")])
print(total_unit)
sales_pie <- c(18860169, 9741271, 14379603, 5139979) #total of each unit respectively 
categories <- c("Product 1", "Product 2", "Product 3", "Product 4")
# Plotting the Pie Chart
pie(sales_pie, labels = categories, main = "Unit Sales Distribution", col = rainbow(length(categories)))

# Total Revenue Product 1, Product 2, Product 3, Product 4
total_sales <- colSums(sales[, c("V7", "V8", "V9", "V10")])
print(total_sales)
sales_pie1 <- c(59786736, 61759658, 77937448, 36648050) #total sales respectively 
categories1 <- c("Product 1", "Product 2", "Product 3", "Product 4")
# Plotting the Pie Chart
pie(sales_pie1, labels = categories1, main = "Sales Revenue Distribution", col = rainbow(length(categories1)))


# Function to plot a bar chart for unit product
plot_bar_chart <- function(sales, columns, stri, str1, val) {
  
  # Aggregate sales for each product by year, by sum or mean
  if (val == 'sum') {
    sales_by_year <- aggregate(sales[columns], by=list(sales$Year), sum)
  } else if (val == 'mean') {
    sales_by_year <- aggregate(sales[columns], by=list(sales$Year), mean)
  }
  
  # Reshape the data for plotting
  sales_by_year_long <- reshape2::melt(sales_by_year, id.vars="Group.1", variable.name="Product", value.name="Sales")
  
  # Create a bar chart
  ggplot(sales_by_year_long, aes(x=factor(Group.1), y=Sales, fill=Product)) +
    geom_bar(stat="identity", position="dodge") +
    labs(x='Year', y=stri, title=paste(stri, "by", str1)) +
    theme_minimal()
}

# Use the plot_bar_chart function
plot_bar_chart(sales, c('V3', 'V4', 'V5', 'V6'), 'Total Unit Sales', 'Year', 'sum')

plot_bar_chart(sales, c('V3', 'V4', 'V5', 'V6'), 'Mean Unit Sales', 'Year', 'mean')

# Function to plot a bar chart by Revenue of Product
plot_bar_chart1 <- function(sales, columns, stri, str1, val) {
  
  # Aggregate sales for each product by year, by sum or mean
  if (val == 'sum') {
    sales_by_year1 <- aggregate(sales[columns], by=list(sales$Year), sum)
  } else if (val == 'mean') {
    sales_by_year1 <- aggregate(sales[columns], by=list(sales$Year), mean)
  }
  
  # Reshape the data for plotting
  sales_by_year_long1 <- reshape2::melt(sales_by_year1, id.vars="Group.1", variable.name="Product", value.name="Sales")
  
  # Create a bar chart
  ggplot(sales_by_year_long1, aes(x=factor(Group.1), y=Sales, fill=Product)) +
    geom_bar(stat="identity", position="dodge") +
    labs(x='Year', y=stri, title=paste(stri, "by", str1)) +
    theme_minimal()
}

# Use the plot_bar_chart function
plot_bar_chart1(sales, c('V7', 'V8', 'V9', 'V10'), 'Total Revenue', 'Year', 'sum')

plot_bar_chart1(sales, c('V7', 'V8', 'V9', 'V10'), 'Average Revenue', 'Year', 'mean')

library(ggplot2)


# Function to plot a bar chart by Revenue of Product by Month
plot_bar_chart2 <- function(sales, columns, stri, str1, val) {
  
  # Aggregate sales for each product by month, by sum or mean
  if (val == 'sum') {
    sales_by_month <- aggregate(sales[columns], by=list(sales$Month_Name), sum)
  } else if (val == 'mean') {
    sales_by_month <- aggregate(sales[columns], by=list(sales$Month_Name), mean)
  }
  
  # Reshape the data for plotting
  sales_by_month_long <- reshape2::melt(sales_by_month, id.vars="Group.1", variable.name="Product", value.name="Sales")
  
  # Convert 'Month_Name' to a factor with correct levels
  sales_by_month_long$Group.1 <- factor(sales_by_month_long$Group.1, levels = month.name)
  
  # Create a bar chart
  ggplot(sales_by_month_long, aes(x=Group.1, y=Sales, fill=Product)) +
    geom_bar(stat="identity", position="dodge") +
    labs(x='Month', y=stri, title=paste(stri, "by", str1)) +
    theme_minimal()
}

# Use the plot_bar_chart function
plot_bar_chart2(sales, c('V7', 'V8', 'V9', 'V10'), 'Total Revenue', 'Month', 'sum')

plot_bar_chart2(sales, c('V7', 'V8', 'V9', 'V10'), 'Average Revenue', 'Month', 'mean')

.........................................................................
# Function to plot a bar chart of Product Unit by Month
plot_bar_chart3 <- function(sales, columns, stri, str1, val) {
  
  # Aggregate sales for each product by month, by sum or mean
  if (val == 'sum') {
    sales_by_month <- aggregate(sales[columns], by=list(sales$Month_Name), sum)
  } else if (val == 'mean') {
    sales_by_month <- aggregate(sales[columns], by=list(sales$Month_Name), mean)
  }
  
  # Reshape the data for plotting
  sales_by_month_long1 <- reshape2::melt(sales_by_month, id.vars="Group.1", variable.name="Product", value.name="Sales")
  
  # Convert 'Month_Name' to a factor with correct levels
  sales_by_month_long1$Group.1 <- factor(sales_by_month_long1$Group.1, levels = month.name)
  
  # Create a bar chart
  ggplot(sales_by_month_long1, aes(x=Group.1, y=Sales, fill=Product)) +
    geom_bar(stat="identity", position="dodge") +
    labs(x='Month', y=stri, title=paste(stri, "by", str1)) +
    theme_minimal()
}

# Use the plot_bar_chart function
plot_bar_chart3(sales, c('V3', 'V4', 'V5', 'V6'), 'Total Unit Sold', 'Month', 'sum')

plot_bar_chart3(sales, c('V3', 'V4', 'V5', 'V6'), 'Average Unit Sold', 'Month', 'mean')
......................................................................................

# Function to plot a bar chart of Product Unit by Day 
plot_bar_chart4 <- function(sales, columns, stri, str1, val) {
  
  # Aggregate sales for each product by month, by sum or mean
  if (val == 'sum') {
    sales_by_day <- aggregate(sales[columns], by=list(sales$Day_Of_Week), sum)
  } else if (val == 'mean') {
    sales_by_day <- aggregate(sales[columns], by=list(sales$Day_Of_Week), mean)
  }
  
  # Reshape the data for plotting
  sales_by_day_long1 <- reshape2::melt(sales_by_day, id.vars="Group.1", variable.name="Product", value.name="Sales")
  
  # Convert 'Day Name' to a factor with correct levels
  sales_by_day_long1$Group.1 <- factor(sales_by_day_long1$Group.1, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
  # Create a bar chart
  ggplot(sales_by_day_long1, aes(x=Group.1, y=Sales, fill=Product)) +
    geom_bar(stat="identity", position="dodge") +
    labs(x='Day Of Week', y=stri, title=paste(stri, "by", str1)) +
    theme_minimal()
}

# Use the plot_bar_chart function
plot_bar_chart4(sales, c('V3', 'V4', 'V5', 'V6'), 'Total Unit Sold', 'Day', 'sum')

plot_bar_chart4(sales, c('V3', 'V4', 'V5', 'V6'), 'Average Unit Sold', 'Day', 'mean')
........................................................................................

# Function to plot a bar chart of Sales Revenue by Day 
plot_bar_chart5 <- function(sales, columns, stri, str1, val) {
  
  # Aggregate sales revenue by month, by sum or mean
  if (val == 'sum') {
    sales_by_day <- aggregate(sales[columns], by=list(sales$Day_Of_Week), sum)
  } else if (val == 'mean') {
    sales_by_day <- aggregate(sales[columns], by=list(sales$Day_Of_Week), mean)
  }
  
  # Reshape the data for plotting
  sales_by_day_long2 <- reshape2::melt(sales_by_day, id.vars="Group.1", variable.name="Product", value.name="Sales")
  
  # Convert 'Day Name' to a factor with correct levels
  sales_by_day_long2$Group.1 <- factor(sales_by_day_long2$Group.1, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
  # Create a bar chart
  ggplot(sales_by_day_long2, aes(x=Group.1, y=Sales, fill=Product)) +
    geom_bar(stat="identity", position="dodge") +
    labs(x='Day Of Week', y=stri, title=paste(stri, "by", str1)) +
    theme_minimal()
}

# Use the plot_bar_chart function
plot_bar_chart5(sales, c('V7', 'V8', 'V9', 'V10'), 'Total Sales Revenue', 'Day', 'sum')

plot_bar_chart5(sales, c('V7', 'V8', 'V9', 'V10'), 'Average Sales', 'Day', 'mean')
....................................................................................

library(reshape2)

# Extracting unit sales columns (V3 to V6)
unit_columns <- sales[, c("V3", "V4", "V5", "V6")]

# Calculating the correlation matrix
correlation_matrix <- cor(unit_columns)

# Printing the correlation matrix
print(correlation_matrix)
# Melt the correlation matrix for plotting
melted_corr <- melt(correlation_matrix)
# Create a heatmap
ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "orange", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  labs(title = "Correlation Matrix Heatmap",
       x = "Products", y = "Products")

# Extracting sales revenue columns (V7 to V10)
revenue_columns <- sales[, c("V7", "V8", "V9", "V10")]

# Calculating the correlation matrix
correlation_matrix1 <- cor(revenue_columns)

# Printing the correlation matrix
print(correlation_matrix1)
# Melt the correlation matrix for plotting
melted_corr1 <- melt(correlation_matrix1)
# Create a heatmap
ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "green", high = "black", mid = "pink", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  labs(title = "Correlation Matrix Heatmap",
       x = "Revenue", y = "Revenue")

...........................................
#Generating new columns Feature Engineering 
View(sales)

set.seed(123) 

sales$num_vehicles <- sample(5:20, nrow(sales), replace = TRUE)

sales$total_revenue <- rowSums(sales[, c("V7", "V8", "V9", "V10")], na.rm = TRUE)
sales$wages <- runif(nrow(sales), min = 0.1, max = 0.2) * sales$total_revenue

........................................................................
install.packages("ggplot2")
library(ggplot2)
install.packages("forecast")
library(forecast)


# Convert 'V2' to POSIXct
sales$V2 <- as.POSIXct(sales$V2)

# Subset data for training (up to 2023-02-03)
train_data <- sales[sales$V2 < as.POSIXct("2023-02-04"), ]
# Create a time series object
sales_ts <- ts(train_data$V3, frequency = 365)  # Adjust frequency based on your data
# Fit an ARIMA model
arima_model <- auto.arima(sales_ts)
# Forecast sales
forecast_result <- forecast(arima_model, h = 330)  # Forecast for 330 days (2023-02-04 to 2023-12-31)
# Plot the forecast
plot(forecast_result, main = "Sales Forecast from 2023-02-04 to 2023-12-31", xlab = "Date", ylab = "Unit Sales")
# Extract forecasted values
forecast_values <- forecast_result$mean
forecast_values
View(sales)
forecast_result

..................................................................

#trying Correlation 
install.packages("psych")
library(psych)


cor(sales[c("v7","v8","v8", "v10", "wages", "num_vehicles")])
pairs(sales[c("V7","V8","V9", "V10", "wages", "num_vehicles")])
pairs.panels(sales[c("V7","V8","V9", "V10", "wages", "num_vehicles")])

model_test <- lm(wages ~ V7+V8+V9+V10+num_vehicles, data = sales)
model_test
summary(model_test)

#Check normality ( Shapiro-Wilk Test)
 qqnorm(model_test$residuals);qqline(model_test$residuals, col=2)
 
 shapiro.test(model_test$residuals)

 #Check Equal Variance
 plot(model_test$fitted.values, model_test$residuals)
 abline(0,0, col = 3)
 .............................................. .... 
 ## Identifying Outliers
 
 # Select relevant columns for boxplots
 boxplot_data <- sales[, c("V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "wages","total_revenue","num_vehicles")]
 
 # Create boxplots
 par(mfrow = c(2, 1))  # Arrange plots in two rows
 
 # Boxplots for product unit sold (V3 to V6)
 boxplot(boxplot_data[, 1:4], main = "Product Unit Sold ")
 
 # Boxplots for revenue (V7 to V10)
 boxplot(boxplot_data[, 5:8], main = "Revenue of Each Product")
 
 # Boxplots for Wages
 boxplot(boxplot_data[, 9], main = " Wages ")
 
 # Boxplots for revenue 
 boxplot(boxplot_data[, 10], main = " Total Revenue ")
 
 # Boxplots for Number of Vehicles Used
 boxplot(boxplot_data[, 11], main = " Number of Delivery Vehicle ") 
        
 # Reset the layout
 par(mfrow = c(1, 1))
 
...................................................................... 
## Checking correlation between No. of Vehicle and Total Revenue for Question 4

# Check the correlation between the number of vehicles and total revenue
correlation_result_VR <- cor(sales$num_vehicles, sales$total_revenue, method = "pearson")

# Print the correlation result
print(correlation_result_VR)

# You can also visualize the relationship using a scatter plot
plot(sales$num_vehicles, sales$total_revenue, main = "Scatter Plot: Vehicles vs. Total Sales", 
     xlab = "Number of Vehicles", ylab = "Total Sales")
cor.test(sales$num_vehicles,sales$total_revenue)
.............................................................. 
## Predicting total revenue by total units sold ## Question 5

# Create a new variable total_units_sold for each product
sales$total_units_sold <- rowSums(sales[, c("V3", "V4", "V5", "V6")])

# Fit a linear regression model
linear_model <- lm(total_revenue ~ total_units_sold, data = sales)

# Print the summary of the regression model
summary(linear_model)

# Make predictions using the model
sales$predicted_revenue <- predict(linear_model, newdata = sales)

# Visualize the regression line
plot(sales$total_units_sold, sales$total_revenue, main = "Linear Regression: Revenue vs. Units Sold",
     xlab = "Total Units Sold", ylab = "Total Revenue")
abline(linear_model, col = "red")

# Evaluate the model and check the residuals
plot(linear_model, which = 1)  # Residuals vs. Fitted
plot(linear_model, which = 2)  # Normal Q-Q plot

# You can also use the predict function to get predictions for new data
new_data <- data.frame(total_units_sold = c(100, 200, 300))
predicted_revenue_new <- predict(linear_model, newdata = new_data)
print(predicted_revenue_new)
...........................................
#Question 1 hypothesis 
# Convert the 'Day_Of_Week' variable to a factor to ensure correct analysis
sales$Day_Of_Week <- factor(sales$Day_Of_Week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Perform ANOVA
anova_result <- aov(total_units_sold ~ Day_Of_Week, data = sales)

# Summarize the ANOVA results
summary(anova_result)

summary_result <- summary(anova_result)
# Extract the p-value
p_value <- summary_result[[1]]$`Pr(>F)`[1]
p_value

# Boxplot to visualize the distribution of total units sold across days of the week
boxplot(total_units_sold ~ Day_Of_Week, data = sales, main = "Total Units Sold by Day of the Week", xlab = "Day of the Week", ylab = "Total Units Sold")


# Add stripchart to show individual data points
stripchart(total_units_sold ~ Day_Of_Week, data = sales, method = "jitter", add = TRUE, col = "blue")
.........................................................................

## Question 2: Weekdays VS Weekends 
# Convert Day_Of_Week column to a factor
sales$Day_Of_Week <- factor(sales$Day_Of_Week, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Split the data into two groups: Weekend (Saturday and Sunday) and Weekday (Monday to Friday)
weekend_sales <- sales[sales$Day_Of_Week %in% c("Friday","Saturday", "Sunday"), ]
weekday_sales <- sales[sales$Day_Of_Week %in% c("Monday", "Tuesday", "Wednesday", "Thursday"), ]

# Perform t-test
t_test_result <- t.test(weekend_sales$total_revenue, weekday_sales$total_revenue)

# Display the results
print(t_test_result)
...................................
#Question 3: 

correlation_WR <- cor(sales$wages, sales$total_revenue)

# Display the correlation coefficient
print(paste("Correlation Coefficient:", correlation_WR))
# Testing 
correlation_test_WR <- cor.test(sales$wages, sales$total_revenue)

# Display the test result
print(correlation_test_WR)

# Scatter plot of revenue vs. wages
plot(sales$wages, sales$total_revenue, main = "Scatter Plot of Revenue vs. Wages",
     xlab = "Wages", ylab = "Total Revenue", col = "blue", pch = 16)
# Add a linear regression line
abline(lm(total_revenue ~ wages, data = sales), col = "red")

install.packages("corrplot")
library(corrplot)
install.packages("psych")
library(psych)

# Calculate the correlation matrix
cor_matrix <- cor(sales[c("wages", "total_revenue")])

# Create a heatmap
corrplot(cor_matrix, method = "color")
pairs.panels(sales[c("wages", "total_revenue", "num_vehicles","total_units_sold")])
................................................................ 

#  total revenue data is in a variable named 'total_revenue' and months are in 'Month'
# Question 4 hypothesis of revenue with months 
anova_result <- aov(total_revenue ~ Month_Name, data = sales)

# Print summary
summary(anova_result)


# Boxplot
# Define the order of months
month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# Convert 'Month' to a factor with specified order
sales$Month_Name <- factor(sales$Month_Name, levels = month_order)
boxplot(total_revenue ~ Month_Name, data = sales, main = "Total Revenue Across Months", xlab = "Month", ylab = "Total Revenue")


# You can customize these lines based on your significance level
abline(h = c(50000, 100000), col = "red", lty = 2)
..........................................................................
#Question 8
# Assuming 'sales' is your dataset
sales$V2 <- as.Date(sales$V2)  # Convert the date column to Date type

# Create a time series object
sales_ts <- ts(sales$total_units_sold, frequency = 12)  # Assuming monthly data

# Plot the time series
plot(sales_ts, main = "Product Unit Sales Over Time", ylab = "Total Units Sold")

# ACF plot to check for seasonality
acf(sales_ts, main = "Autocorrelation Function (ACF)")
.................
# question 8 Stl

# Assuming 'sales' is your dataset
sales$V2 <- as.Date(sales$V2)  # Convert the date column to Date type

# Create a time series object
sales_ts <- ts(sales$total_units_sold, frequency = 12)  # Assuming monthly data

# Decompose the time series using stl
sales_decomposed <- stl(sales_ts, s.window = "periodic")

# Plot the components
plot(sales_decomposed)

# Plot the seasonally adjusted time series
plot(sales_decomposed$time.series[, "seasonal"], main = "Seasonally Adjusted Product Unit Sales")

# Plot the trend component
plot(sales_decomposed$time.series[, "trend"], main = "Trend of Product Unit Sales")

# Plot the remainder (residuals)
plot(sales_decomposed$time.series[, "remainder"], main = "Residuals of Product Unit Sales")
.................................... ....

#try 2
# Create a time series object
sales_ts <- ts(sales$total_units_sold, start = c(2010, 6),end = c(2023,2), frequency = 12)

# Decompose the time series using stl and Plot
plot(stl(sales_ts, s.window = "periodic"), plot.type = "single", main = " Total Units Sold Time Series Decomposition")

..
 # For Revenue 
#Create a time series object
sales_tsR <- ts(sales$total_revenue, start = c(2010, 6),end = c(2023,2), frequency = 12)

# Decompose the time series using stl and Plot
plot(stl(sales_tsR, s.window = "periodic"), plot.type = "single", main = " Total Revenue Time Series Decomposition")
......................................    
# Comparing seasonality between normal data and scaled data 

# Convert the date column to Date format if it's not already
sales$V2 <- as.Date(sales$V2)

# Create a time series object for original data
sales_ts <- ts(sales$total_units_sold, start = c(2010, 6),end = c(2023,2), frequency = 12)

# Decompose the time series using stl for original data
sales_stl <- stl(sales_ts, s.window = "periodic")

# Plot the decomposition for original data
plot(sales_stl, main = "Original Data")

# Create a time series object for scaled data
sales_scaled_ts <- ts(scale(sales$total_units_sold), start = c(2010, 6), end = c(2023,2), frequency = 12)

# Decompose the time series using stl for scaled data
sales_scaled_stl <- stl(sales_scaled_ts[, 1], s.window = "periodic")  # Extract only the numeric column for decomposition

# Plot the decomposition for scaled data
plot(sales_scaled_stl, main = "Scaled Data")
.............................................................  

#Question 9

# install.packages("forecast")
install.packages("forecast")
library(forecast)

# Assuming sales is your data frame
sales$V2 <- as.Date(sales$V2)

# Create a time series object for total revenue
revenue_ts <- ts(sales$total_revenue, start = c(2010, 6), end = c(2023,2), frequency = 12)

# Fit an ARIMA model to the historical data (replace this with your actual model)
forecast_model_revenue <- auto.arima(revenue_ts)
print(forecast_model_revenue)
plot.ts(forecast_model_revenue$residuals)

# Generate forecasts for next 12 months
forecast_values_revenue <- forecast(forecast_model_revenue, h = 12)  # Adjust h based on your needs
print(forecast_values_revenue)
plot(forecast_values_revenue)

# Repeat the process for wages
wages_ts <- ts(sales$wages, start = c(2010, 6), end = c(2023,2), frequency = 12)
# Arima Model
forecast_model_wages <- auto.arima(wages_ts) 
print(forecast_model_wages)
plot(forecast_model_wages$residuals)
 # Forecast for next 12 months
forecast_values_wages <- forecast(forecast_model_wages, h = 12*2)  # Adjust h based on your needs
print(forecast_values_wages)
plot(forecast_values_wages)
...................................................................


summary(sales)
install.packages("psych")
library(psych)

describe(sales)

pairs.panel
