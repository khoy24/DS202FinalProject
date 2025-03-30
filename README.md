
### Research topic: Customer Purchases Behavior

### Team members: Kaitlyn Hoyme and Christopher Moseley

### Data:

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
```

### Questions to be addressed:

- How does gender affect customer purchasing history
