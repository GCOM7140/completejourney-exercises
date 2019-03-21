# completejourney-exercises

The exercises and solutions in this repository are designed to help you practice
using the tidyverse to import, tidy, transform, and visualize customer data. For
these exercises, you will analyze data available in the completejourney package
(details below). Reviewing relevant chapters in [R4DS][r4ds] carefully before
attempting these exercises will ensure that the requisite skills you need to
complete them are top of mind.

## Overview of the completejourney Package
The completejourney package is an R data package that was created so you can
load the full suite of complete journey datasets, also available at
[8451.com/area51][area51], with a single line of code. Originating from 84.51’s
Complete Journey Study, it includes data representing one year's worth of
grocery store shopping transactions for a group of 2,469 households who were
frequent shoppers at a grocery retailer. These data capture all of these
households’ purchases, not just those from a limited number of categories. For
certain households, demographic information as well as direct marketing contact
history are available. The completejourney package comes with the following
datasets:

- `transactions`: receipt information for purchases made by households
- `products`: product metadata (brand, description, etc.)
- `demographics`: household demographic data (age, income, family size, etc.)
- `campaigns`: campaigns received by each household
- `campaign_descriptions`: campaign metadata (length of time active)
- `coupons`: coupon metadata (UPC code, campaign, etc.)
- `coupon_redemptions`: coupon redemptions (household, day, UPC code, campaign)
- `promotions`: product placement in mailers and in stores corresponding to
advertising campaigns

More information on the completejourney package can be found at:
[bradleyboehmke.github.io/completejourney][completejourney].

Special thanks to [Bradley Boehmke][brad], [Steve Mortimer][steve], and
[Jonathan Eman][jonathan] for making the completejourney package possible!

## Installing the Required Packages
For the most part, you will only need the tidyverse and completejourney packages
to complete these exercises. While you probably have the tidyverse installed on
your machine (run `install.packages("tidyverse")` if you do not), you likely
need to install the completejourney package, which can be done with:

``` r
# install.packages("devtools")
devtools::install_github("bradleyboehmke/completejourney")
```

## Recommended Preparation
We wrote the exercises and solutions in this repository in a way that emphasizes
"tidy" data principles, the tidyverse, and other practices covered in [R for
Data Science][r4ds]. Again, reviewing relevant parts of R4DS carefully *before*
attempting to complete these exercises is the best way to ensure that you grow
as a data scientist through the process of working on them.

## Working on the Exercises
Below are links to the exercises in this repository. You can also find them by
navigating to the [exercises][exercises] folder of this repository and clicking
on the files ending in `.md`.

Exercises:

 1. [Data Transformation][data transformation exercises]
 2. [Data Visualization][data visualization exercises]
 3. [Exploratory Data Analysis][eda exercises]
 4. [Data Wrangling][data wrangling exercises]
<!---
 5. [Brandefy Private Label Case][brandefy case exercises]
-->

## Reviewing the Solutions
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
<!---
 5. [Brandefy Private Label Case][brandefy case solutions]
-->

## Submitting your Work

Add your answers to the [submissions] folder for submission purposes. Please use
the following file-name convention for your submissions:

- 01_cj-data-transformation-lastname-firstname (.R or .R/.md or .Rmd / .md)
- 02_cj-data-visualization-lastname-firstname (.R or .R/.md or .Rmd / .md)
- 03_cj-exploratory-data-analysis-lastname-firstname (.R or .R/.md or .Rmd /
.md)
- 04_cj-data-wrangling-lastname-firstname (.R or .R/.md or .Rmd / .md)


[area51]: https://www.8451.com/area51/
[brandefy case exercises]: https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/05-brandefy-private-label-case-exercises.md#brandefy-private-label-case-exercises
[completejourney]: https://bradleyboehmke.github.io/completejourney/
[brandefy case solutions]: https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/05-brandefy-private-label-case-solutions.md#brandefy-private-label-case-solutions
[data transformation exercises]: https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/01-data-transformation-exercises.md#data-transformation-exercises
[data transformation solutions]: https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/01-data-transformation-solutions.md#data-transformation-solutions
[data visualization exercises]: https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/02-data-visualization-exercises.md#data-visualization-exercises
[data visualization solutions]: https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/02-data-visualization-solutions.md#data-visualization-solutions
[data wrangling exercises]: https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/04-data-wrangling-exercises.md#data-wrangling-exercises
[data wrangling solutions]: https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/04-data-wrangling-solutions.md#data-wrangling-solutions
[eda exercises]: https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/03-exploratory-data-analysis-exercises.md#exploratory-data-analysis-eda-exercises
[eda solutions]: https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/03-exploratory-data-analysis-solutions.md#exploratory-data-analysis-eda-solutions
[brad]: https://github.com/bradleyboehmke
[exercises]: https://github.com/GCOM7140/completejourney-exercises/tree/master/exercises
[jonathan]: https://github.com/jonathan-eman
[r4ds]: http://r4ds.had.co.nz/index.html
[steve]: https://github.com/StevenMMortimer
[submissions]: https://github.com/GCOM7140/completejourney-exercises/tree/master/submissions
[solutions]: https://github.com/GCOM7140/completejourney-exercises/tree/master/solutions