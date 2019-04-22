library(data.table)
library(bit64) # reading 64-bit values with fread
library(dplyr)
library(rfm)
library(tibble)
library(broom) # extracting p-values using glance()
library(purrr) # pluck & map
options(scipen = 1e9)

# Read & format the data
sales <- fread("ta_feng.csv", sep = ",")
sales$TRANSACTION_DT <- as.Date(sales$TRANSACTION_DT, format = "%m/%d/%Y")
sales$AMOUNT <- as.numeric(sales$AMOUNT)
sales$SALES_PRICE <- as.numeric(sales$SALES_PRICE)

# Remove outliers for plotting
sales_no_outliers <- sales %>% 
  filter(SALES_PRICE < quantile(.$SALES_PRICE, 0.99),
         AMOUNT < quantile(.$AMOUNT, 0.999))

# Plot totals and quantities
hist(sales_no_outliers$SALES_PRICE, breaks = 50)
hist(sales_no_outliers$AMOUNT, breaks = 50)

# Amount of customers:
length(unique(sales$CUSTOMER_ID))

# Mean quantity of total items bought by customer
sales %>% 
  group_by(CUSTOMER_ID) %>% 
  summarise(qty = sum(AMOUNT)) %>% 
  .$qty %>%
  mean(na.rm = T)

# Make a new data frame containing days since last shopping time,
# frequency and total amount spent for RFM analysis
rfm_data <- sales %>% 
  group_by(CUSTOMER_ID) %>% 
  summarise(r = TRANSACTION_DT[which.max(TRANSACTION_DT)],
            f = n(),
            m = sum(SALES_PRICE))

# A faster way is calculating the date difference outside the pipes
max_date <- max(sales$TRANSACTION_DT)
rfm_data$r <- as.numeric(max_date - rfm_data$r)

# Classify customers by RFM score and plot a heatmap
rfm_scores <- as.list(rfm_table_customer(rfm_data, CUSTOMER_ID, f, r, m, max_date))
rfm_heatmap(rfm_scores)
# Remove excessive 1x1 columns
rfm_scores_df <- as.data.frame(rfm_scores$rfm)

#-------------------------------------------------------------------
# Add purchase prices with two decimals
sales$price <- as.numeric(round(sales$SALES_PRICE / sales$AMOUNT, 2))

# Get the prices and demands for each product that has had multiple prices
product_list <- sales %>% 
  group_by(PRODUCT_ID) %>% 
  summarise(prices = list(price),
            demands = list(AMOUNT),
            var = var(price, na.rm = T) / mean(price, na.rm = T),
            n = n()) %>% 
  filter(var > 0) %>% 
  as.tibble()

# Do linear models to obtain coefficients
lms <- product_list %>% 
  split(.$PRODUCT_ID) %>% 
  map(~ safely(lm)(pluck(demands, 1) ~ pluck(prices, 1), data = .)$result)

# Get the second coefficient from each list
coefs <- lms %>% 
  map(coefficients) %>%
  sapply("[[", 2)

# Get p-values for the models
pvalues <- lms %>% 
  map(glance) %>% 
  map("p.value")

# Combine the obtained values
pvalues <- unlist(pvalues)
product_list <- as.tibble(cbind(product_list, cbind(coefs, as.data.frame(pvalues))))

# Calculate elasticities
product_list <- product_list %>% 
  group_by(PRODUCT_ID) %>% 
  summarise(elasticity = coefs *
              (mean(as.numeric(pluck(prices, 1))) /
                 mean(as.numeric(pluck(demands, 1))))) %>% 
  inner_join(product_list, ., by = "PRODUCT_ID")

# Filter those with bad p-values (optional)
elasticities <- product_list %>% 
  filter(pvalues < 0.05) %>% 
  select(PRODUCT_ID,
         n,
         pvalues,
         elasticity)

# Plot the elasticities after removing outliers
elasticities_no_outliers <- elasticities %>% 
  filter(elasticity < 100, elasticity > -100)

hist(elasticities_no_outliers$elasticity, breaks = 100, main = "Distribution of elasticities")