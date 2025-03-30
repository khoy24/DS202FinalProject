
### Research topic: Customer Purchases Behavior

### Team members: Kaitlyn Hoyme and Christopher Moseley

### Data:

This dataset includes information on customer purchasing behavior.  
There are 100000 rows of 12 variables.  
Variables are:  
- age : age of customer  
- gender : gender of customer  
- income : income of customer  
- education : education level of customer  
- region : region of customers residence  
- loyalty_status : loyalty status of customer  
- purchasing_frequency : frequency of purchases by customer  
- purchase_amount : amount spent by customer in each purchase  
- product_category : category of purchased product  
- promotion_usage : 0 means the customer did not use promotional offers,
1 means the customer did use promotional offers  
- satisfaction_score : satisfaction score of customer

This data was originally simulated in R and the dataset was obtained
through Kaggle, the data mimics real scenarios.

``` r
library(dplyr)
library(ggplot2)

#Data: description of your data set, first data cleaning steps, marginal summaries;

url <- "https://raw.githubusercontent.com/khoy24/DS202FinalProject/main/customer_data.csv"
data <- read.csv(url)
 
any(is.na(data))
```

    ## [1] FALSE

``` r
# no missing entries

duplicateRows <- data %>% filter(duplicated(.))
print(nrow(duplicateRows))
```

    ## [1] 0

``` r
#no duplicate rows



calculate_summary <- function(column) {
  c(
    mean = mean(column, na.rm = TRUE),
    median = median(column, na.rm = TRUE),
    sd = sd(column, na.rm = TRUE),
    min = min(column, na.rm = TRUE),
    max = max(column, na.rm = TRUE),
    range = diff(range(column, na.rm = TRUE))  # Range is the difference between max and min
  )
}

summary_stats <- data.frame(
  age = calculate_summary(data$age),
  income = calculate_summary(data$income),
  purchase_amount = calculate_summary(data$purchase_amount)
)

print(summary_stats)
```

    ##              age   income purchase_amount
    ## mean   30.003260 27516.27        9634.791
    ## median 30.000000 27584.50        9452.000
    ## sd      4.480535 12996.78        4799.339
    ## min    12.000000  5000.00        1118.000
    ## max    49.000000 50000.00       26204.000
    ## range  37.000000 45000.00       25086.000

### Questions to be addressed:

- How does gender affect customer purchasing behavior?
- How does region affect customer purchasing behavior?
- How does education level affect customer purchasing behavior?
- How does income affect customer purchasing behavior?
- How does age affect customer purchasing behavior?
- What category of products are more popular among various age groups,
  genders, and regions?
- How does loyalty status of the customer affect purchasing behavior?
- How do various factors (age,gender,region,education level,income)
  affect whether or not a customer has a loyalty status?
- What is the relationship between purchase frequency and purchase
  amount?
- How does promotion usage affect customer behavior?
- How do various factors (age,gender,region,education level,income)
  affect customer satisfaction score?
