library(data.table)
library(bit64) # reading 64-bit values with fread
library(dplyr)
library(rfm)
library(tibble)
library(broom) # extracting p-values using glance()
options(scipen = 1e9)

# Read & format the data
sales <- fread("ta_feng.csv", sep = ";")
sales$date <- as.Date(sales$date)

# Remove outliers for plotting
sales_no_outliers <- sales %>% 
  filter(total < quantile(.$total, 0.99),
         quantity < quantile(.$quantity, 0.999))
# Plot totals and quantities
hist(sales_no_outliers$total, breaks = 50)
hist(sales_no_outliers$quantity, breaks = 50)

# Amount of customers:
length(unique(sales$customer_id))

# Mean quantity of total items bought by customer
sales %>% 
  group_by(customer_id) %>% 
  summarise(qty = sum(quantity)) %>% 
  .$qty %>%
  mean(na.rm = T)

# Make a new data frame containing days since last shopping time,
# frequency and total amount spent for RFM analysis
rfm_data <- sales %>% 
  group_by(customer_id) %>% 
  summarise(r = as.Date(date[which.max(date)]),
            f = n(),
            m = sum(total))
# A faster way is calculating the date difference outside the pipes
max_date <- max(sales$date)
rfm_data$r <- max_date - rfm_data$r

  
# Classify customers by RFM score and plot a heatmap
rfm_scores <- as.list(rfm_table_customer(rfm_data, customer_id, f, r, m, max_date))
rfm_heatmap(rfm_scores)
# Remove excessive 1x1 columns
rfm_scores_df <- as.data.frame(rfm_scores$rfm)

#-------------------------------------------------------------------
# Add purchase prices with two decimals
sales$price <- as.numeric(round(sales$total / sales$quantity, 2))

# Get the prices and demands for each product that has had multiple prices
product_list <- sales %>% 
  group_by(proudct_id) %>% 
  summarise(prices = list(price),
            demands = list(quantity),
            var = var(price, na.rm = T) / mean(price, na.rm = T),
            n = n()) %>% 
  filter(var > 0) %>% 
  as.tibble()

product_list$r_squared <- NA
product_list$p_value <- NA
product_list$elasticity <- NA

# Calculate elasticities for each product
for(i in 1:nrow(product_list)){
temp <- (cbind(as.data.frame(product_list$prices[i]), as.data.frame(product_list$demands[i])))
names(temp) <- c("prices", "demands")
temp <- temp %>% 
  group_by(prices) %>% 
  summarise(demand = sum(demands))

lm_temp <- lm(demand ~ prices, temp)

product_list$r_squared[i] <- summary(lm_temp)$r.squared
product_list$p_value[i] <- glance(lm_temp)$p.value
product_list$elasticity[i] <- lm_temp$coefficients["prices"] *
                           (mean(temp$prices) / mean(temp$demand))
}

elasticities <- product_list %>% 
  filter(p_value < 0.05) %>% 
  select(proudct_id,
         n,
         r_squared,
         p_value,
         elasticity)

# Plot the elasticities after removing outliers
elasticities_no_outliers <- elasticities %>% 
  filter(elasticity < 100, elasticity > -100)
hist(elasticities_no_outliers$elasticity, breaks = 100, main = "Distribution of elasticities")
