
# completejourney-exercises

## Package Overview

The `completejourney` package originates from 84.51’s Complete Journey
Study and includes data representing one year of grocery store shopping
transactions for a group of 2,469 households who were frequent shoppers
at a grocery retailer. These data capture all of these households’
purchases, not just those from a limited number of categories. For
certain households, demographic information as well as direct marketing
contact history are available. The `completejourney` package comes with
the following datasets:

  - `transactions`: receipt information for purchases made by households
  - `products`: product metadata (brand, description, etc.)
  - `demographics`: household demographic data (age, income, family
    size, etc.)
  - `campaigns`: campaigns received by each household
  - `campaign_descriptions`: campaign metadata (length of time active)
  - `coupons`: coupon metadata (UPC code, campaign, etc.)
  - `coupon_redemptions`: coupon redemptions (household, day, UPC code,
    campaign)
  - `promotions`: product placement in mailers and in stores
    corresponding to advertising campaigns

The exercises and solutions in this repo are designed to help you
practice using the tidyverse to import, tidy, transform, and visualize
customer data. Reviewing relevant chapters in
[R4DS](http://r4ds.had.co.nz/index.html) carefully for best practices
before attempting these exercises will ensure you have the requisite
skills needed to complete them.

## Table of Contents

  - [Installing the Required
    Packages](#installing-the-required-packages)
  - [Working on the Exercises](#working-on-the-exercises)
  - [Reviewing the Solutions](#reviewing-the-solutions)

### Installing the Required Packages

The only packages you need to complete these exercises are `tidyverse`
and `completejourney`. While you probably have the `tidyverse` installed
on your machine (run `install.packages("tidyverse")` in your console if
you haven’t), you likely need to install the `completejourney` package,
which can be done from GitHub with the following code:

``` r
# install.packages("devtools")
devtools::install_github("bradleyboehmke/completejourney")
```

The `completejourney` package is an R data package that was created to
let you load the full suite of Complete Journey datasets, available at
[8451.com/area51](https://www.8451.com/area51/), as a library with this
single line of code. Special thanks to [Bradley
Boehmke](https://github.com/bradleyboehmke) and [Steve
Mortimer](https://github.com/StevenMMortimer) for making this possible\!

More information on the `completejourney` package can be found at:
[bradleyboehmke.github.io/completejourney](https://bradleyboehmke.github.io/completejourney/).

### Working on the Exercises

Below are links to the exercises. You can also find them by navigating
to the
[exercises](https://github.com/GCOM7140/completejourney-exercises/tree/master/exercises)
folder of this repo and clicking on the files ending in `.md`.

Exercises:

1.  [Data
    Transformation](https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/01-data-transformation-exercises.md)
2.  [Data
    Visualization](https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/02-data-visualization-exercises.md)
3.  [Exploratory Data
    Analysis](https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/03-exploratory-data-analysis-exercises.md)
4.  [Data
    Wrangling](https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/04-data-wrangling-exercises.md)
5.  [Brandefy Private Label
    Case](https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/05-brandefy-private-label-case-exercises.md)

### Reviewing the Solutions

After working through the exercises, have a look at the solutions
provided in the
[solutions](https://github.com/GCOM7140/completejourney-exercises/tree/master/solutions)
folder to compare your work with some answers we developed. There are
usually multiple ways to perform the same operation in R, so don’t be
concerned if your code doesn’t match what’s provided exactly. Focus
instead on ensuring that your output and answers appear to be in the
same ballpark. Below are links to the solution files we developed:

Solutions:

1.  [Data
    Transformation](https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/01-data-transformation-solutions.md)
2.  [Data
    Visualization](https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/02-data-visualization-solutions.md)
3.  [Exploratory Data
    Analysis](https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/03-exploratory-data-analysis-solutions.md)
4.  [Data
    Wrangling](https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/04-data-wrangling-solutions.md)
5.  [Brandefy Private Label
    Case](https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/05-brandefy-private-label-case-solutions.md)
