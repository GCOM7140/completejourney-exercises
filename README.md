# completejourney-exercises
A collection of exercises and solutions based on the Complete Journey study provided 
by [84.51](http://www.8451.com). The study tracks retail shopping transactions for 
2,500 households over two years.

### Step 1: Installing the Required Packages
All of the exercises can be solved using the `tidyverse` and `completejourney` packages. 
The `completejourney` package is an R data package that has been created so the 
full suite of Complete Journey datasets can be loaded as a library. In order to 
use the data you must first install the package following these steps:

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
`install.packages('devtools')`. More information on the `completejourney` package 
can be found at: https://github.com/GCOM7140/completejourney

### Step 2: Working on the Exercises
Below are links to the exercises. You can also find them by navigating to the [exercises](https://github.com/GCOM7140/completejourney-exercises/tree/master/exercises) 
folder of this repository and click on the files ending in `.md` to view. 

Exercises: 

 1. [Data Transformation](https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/01-data-transformation-exercises.md)
 2. [Data Visualization](https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/02-data-visualization-exercises.md)
 3. [Exploratory Data Analysis](https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/03-exploratory-data-analysis-exercises.md)
 4. [Data Wrangling](https://github.com/GCOM7140/completejourney-exercises/blob/master/exercises/04-data-wrangling-exercises.md)

### Step 3: Reviewing the Solutions
After working through the exercises look at the solutions provided in the [solutions](https://github.com/GCOM7140/completejourney-exercises/tree/master/solutions) folder. 
Compare your work with what is provided. There are usually multiple ways to perform 
the same operation in R, so do not be concerned if your code does not match exactly. 
Focus on ensuring that your output and answer appears the same. Below are links to the solutions.

Solutions: 

 1. [Data Transformation](https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/01-data-transformation-solutions.md)
 2. [Data Visualization](https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/02-data-visualization-solutions.md)
 3. [Exploratory Data Analysis](https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/03-exploratory-data-analysis-solutions.md)
 4. [Data Wrangling](https://github.com/GCOM7140/completejourney-exercises/blob/master/solutions/04-data-wrangling-solutions.md)

### Additional Resources
If you would like to review additional material while working through these exercises 
there are two great resources available: 

1. The exercises and solutions are written in a way that emphasizes learning of "tidy" data, 
the tidyverse, and other principles covered in [R4DS](http://r4ds.had.co.nz/index.html). Review 
this material carefully for best practices to use when completing these exercises.

2. The source of the Complete Journey study data is available at: http://www.8451.com/area51/
