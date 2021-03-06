Brandefy Private Label Case Exercises
================

The following 7 questions are based on the Brandefy case study and can be answered using The Complete Journey data. The Complete Journey data is an ideal test bed for better understanding consumer behavior around national and private label brands as Meg Greenhalgh considers building a Brandefy app. There is a column entitled "brand" in the `product` dataset that shows for each product whether it is a "National" or "Private" brand. Start your analysis by loading the `tidyverse`, `scales`, `gridExtra`, and `completejourney` packages.

``` r
# required packages
library(tidyverse)
library(gridExtra)
library(scales)
library(completejourney)
```

------------------------------------------------------------------------

**Question 1**:
Meg Greenhalgh is trying to gauge the overall opportunity for usage of an app that shoppers could use in-store to scan items and see product reviews of similar private label products. Meg Greenhalgh thinks that shoppers are confronted with roughly 10 opportunities per trip to decide between a national or private-label brand. Using the Complete Journey dataset count the number of private label items per basket and plot the distribution as a histogram. This will help to start to get a sense for how many times consumers purchase a private label item per trip. Hint: When calculating the private label items per basket ensure you include the baskets with zero private label items. You can do this by summarizing the baskets using: `summarize(private_item_cnt = sum(brand == 'Private'))`.

------------------------------------------------------------------------

**Question 2**:
The answer to Question 1 is all well and good, but people might only use the Brandefy app the first time they purchase a private label product. Repeat the analysis of Question 1 only counting the first-time purchase of a private label product per household. Hint: Arrange the transactions by `day`, then add a counter using the`row_number()` function to determine the first instance that a `product_id` was purchased by a `household_key`.

------------------------------------------------------------------------

**Question 3**: Meg Greenhalgh saw the histogram you created for Question 2 that only considers first time purchases help and was impressed, but would also like some hard numbers. Calculate the following summary statistics:

1.  The average number of private label products per basket
2.  The average sales value of these products per basket
3.  The percentage of baskets containing at least 1 product
4.  The percentage of baskets containing 0-2 products

------------------------------------------------------------------------

**Question 4**:
Meg Greenhalgh and her team have a limited amount of time and resources to review private label products, so they shifted towards a review club with category captains. What categories should Brandefy prioritize as it expands? To help answer this question, let's first try to prioritize based on the ratio of private to national label products. Meg should have good product review coverage in categories that have a high ratio of private label purchases over national brands. Create a list of the top 10 product categories based on ratio of private label to national product purchases. Here are some suggested steps to analyze:

1.  Inner Join `transaction_data` with `product` datasets to obtain the `commodity_desc` per transaction
2.  Obtain a count of transactions by `commodity_desc` and `brand` using the `count()` function
3.  Use the `spread()` function to create two columns of counts side-by-side (Private and National)
4.  Calculate a private-to-national ratio by dividing the `Private` column by the `National` column
5.  Calculate a column called `total_cnt` so we can filter out any noise related to categories with only a handful of purchases.
6.  Filter the dataset to only the top 10 by private-to-national ratio categories with at least 1,000 total transactions
7.  Arrange the data in descending order by the private-to-national ratio

------------------------------------------------------------------------

**Question 5**:
Meg Greenhalgh would also like to consider prioritizing product categories based on the price gap between national brands and private label brands. Create a list of the top 10 product categories based on price difference of the private and national brand product. Here are some suggested steps to analyze:

1.  Find the count of national and private label transactions in each `commodity_desc`
2.  Filter this table to only include categories with at least 100 national and private label transactions. This can be used later to filter out and only look at price differences between categories which have a critical mass of transactions in both national and private label products.
3.  Inner Join `transaction_data` with `product` datasets to obtain the `commodity_desc` per transaction
4.  Inner Join the filtered counts data with the result from Step 3 by `commodity_desc` to filter down the categories we're considering
5.  Group by `commodity_desc` and `brand` and calculate the average `sales value`
6.  Use the `spread()` function to put the private and national average prices side-by-side and calculate the difference as `price_diff = Private - National`.
7.  Select the top 10 most negative prices using `top_n(-10, price_diff)`.

------------------------------------------------------------------------

**Question 6**:
Meg Greenhalgh is interested in spending habits if the purchaser is also the user of the product. Meg has a theory that people purchase private label brands for their children but not for themselves in many occasions. The `completejourney` dataset provides a distinction for vitamins (children's vs. other types) that we can use to run a preliminary test on this hypothesis. Here are some suggested steps to analyze:

1.  Inner Join `transaction_data` with `product` datasets to obtain the `sub_commodity_desc` per transaction
2.  Filter to only the transactions with "VITAMIN" in the `sub_commodity_desc` field
3.  Summarize three metrics:
    3.1 the total count of transactions being analyzed
    3.2 the count that were private label transactions
    3.3 the percentage of transactions that were private label
4.  Filter out the noise by only considering sub-categories with over 100 total transactions

------------------------------------------------------------------------

**Question 7**:
In the previous question (Question 6) we determined that children's vitamins are the least likely to be purchased as private label compared to all other vitamin types. Run the same analysis again for products with `commodity_desc` = "BABYFOOD". Is this different from what you might have concluded in Question 6?
