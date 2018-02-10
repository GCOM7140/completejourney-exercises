# completejourney-exercises
A collection of exercises and solutions based on the Complete Journey Study

### Step 1: Installing the Data Package
An R data package has been created so that you can load the full suite of 
Complete Journey datasets as a library. In order to use the data you must first 
install the package following these steps: 

1. Navigate to: https://github.com/settings/tokens and create a Personal Access Token. 
The `completejourney` data package is a private repository, which requires you to access 
using a token.
2. Install the package by including the token when you run the command 
`devtools::install_github()` from your R console:

```
devtools::install_github('GCOM7140/completejourney', 
                         auth_token = 'PERSONAL_ACCESS_TOKEN_HERE')
```

**NOTE**: Installing packages from GitHub requires the installation of the `devtools` 
package, which can be installed by running the following command from the R console: 
`install.packages('devtools')`

### Step 2: Using the data
After installing the `completejourney` package, you can load the datasets by running the 
command `library(completejourney)` from the R console. While working through the 
exercises you can reference a dataset by name like this:

```
library(completejourney)
transaction_data %>% 
  group_by(household_key) %>%
  summarize(total_household_spend = SALES_VALUE)
```

### Source
The Complete Journey data is available at: http://www.8451.com/area51/
