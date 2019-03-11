# completejourney-exercises

## Package Overview
The `completejourney` package originates from 84.51’s Complete Journey Study. It
includes data representing one year's worth of grocery store shopping
transactions for a group of 2,469 households who were frequent shoppers at a
grocery retailer. These data capture all of these households’ purchases, not
just those from a limited number of categories. For certain households,
demographic information as well as direct marketing contact history are
available. The `completejourney` package comes with the following datasets:

- `transactions`: receipt information for purchases made by households
- `products`: product metadata (brand, description, etc.)
- `demographics`: household demographic data (age, income, family size, etc.)
- `campaigns`: campaigns received by each household
- `campaign_descriptions`: campaign metadata (length of time active)
- `coupons`: coupon metadata (UPC code, campaign, etc.)
- `coupon_redemptions`: coupon redemptions (household, day, UPC code, campaign)
- `promotions`: product placement in mailers and in stores corresponding to
advertising campaigns

The exercises and solutions in this repository are designed to help you practice
using the tidyverse to import, tidy, transform, and visualize customer data.
Reviewing relevant chapters in [R4DS][r4ds] carefully for best practices before
attempting these exercises will ensure that you have the requisite skills you
need to complete them both effectively and efficiently.

### Installing the Required Packages
You will only need the tidyverse and completejourney packages to complete these
exercises. While you probably have the tidyverse installed on your machine (run
`install.packages("tidyverse")` in your console if you do not), you likely need
to install the completejourney package, which can be done from GitHub with the
following code:

``` r
# install.packages("devtools")
devtools::install_github("bradleyboehmke/completejourney")
```

The completejourney package is an R data package that was created so you can
load the full suite of complete journey datasets, also available at
[8451.com/area51][area51], with a single line of code. Special thanks to
[Bradley Boehmke][brad] and [Steve Mortimer][steve] for making this possible!

More information on the completejourney package can be found at:
[bradleyboehmke.github.io/completejourney][completejourney].

### Working on the Exercises
Below are links to the exercises in this repository. You can also find them by
navigating to the [exercises][exercises] folder of this repository and clicking
on the files ending in `.md`.

Exercises:

 1. [Data Transformation][data transformation exercises]
 2. [Data Visualization][data visualization exercises]
 3. [Exploratory Data Analysis][eda exercises]
 4. [Data Wrangling][data wrangling exercises]
 5. [Brandefy Private Label Case][brandefy case exercises]

### Reviewing the Solutions
After working through these exercises, have a look at the solutions provided in
the [solutions][solutions] folder of this repository to compare your work with
the answers we developed. There are usually multiple ways to perform the same
operation in R, so don’t be concerned if your code doesn’t match what we
developed exactly. Focus instead on ensuring that your output and answers appear
to be in the same ballpark. Below are links to the solution files we developed:

Solutions:

 1. [Data Transformation][data transformation solutions]
 2. [Data Visualization][data visualization solutions]
 3. [Exploratory Data Analysis][eda solutions]
 4. [Data Wrangling][data wrangling solutions]
 5. [Brandefy Private Label Case][brandefy case solutions]

[brandefy case exercises]: https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/05-brandefy-private-label-case-exercises.md
[brandefy case solutions]: https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/05-brandefy-private-label-case-solutions.md
[data transformation exercises]: https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/01-data-transformation-exercises.md#data-transformation-exercises
[data transformation solutions]: https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/01-data-transformation-solutions.md#data-transformation-solutions
[data visualization exercises]: https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/02-data-visualization-exercises.md#data-visualization-exercises
[data visualization solutions]: https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/02-data-visualization-solutions.md#data-visualization-solutions
[data wrangling exercises]: https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/04-data-wrangling-exercises.md#data-wrangling-exercises
[data wrangling solutions]: https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/04-data-wrangling-solutions.md#data-wrangling-solutions
[eda exercises]: https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/03-exploratory-data-analysis-exercises.md#exploratory-data-analysis-eda-exercises
[eda solutions]: https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/03-exploratory-data-analysis-solutions.md#exploratory-data-analysis-eda-solutions
[area51]: https://www.8451.com/area51/
[brad]: https://github.com/bradleyboehmke
[completejourney]: https://bradleyboehmke.github.io/completejourney/
[exercises]: https://github.com/GCOM7140/completejourney-exercises/tree/master/exercises
[r4ds]: http://r4ds.had.co.nz/index.html
[steve]: https://github.com/StevenMMortimer
[solutions]: https://github.com/GCOM7140/completejourney-exercises/tree/master/solutions