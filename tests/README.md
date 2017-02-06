Test Suites for Border Adjustment Calculator
======================================

Test suites in this directory are designed to ensure Shiny server is running 
properly, and to validate numeric outcomes using corner cases. We first
introduce how to execute validation for numeric results. 

Numeric Tests
-------------------

In order to run any of these tests, you need to have 
[R](https://www.r-project.org/) installed. In addition to R, 
[RStudio](https://www.rstudio.com/) is recommended but not necessary. 

After installation:
1. Launch R or RStudio;
2. Open the Package Installer;
3. Search for package `testthat`;
4. Install it and all its dependencies.

Then, in the console type 
```
library(testthat)
test_dir("[YOURDIRECTORY]/Border-adjustment-calculator/tests", filter = 'numeric', reporter="summary")
```
and run. 

We should expect the following output:
```
.............................................
DONE ==============================================================================================================================
```
meaning that all 45 tests have passed. 

Troubleshoot
------------------
Here are some typical errors we might run into:

****Error 1****:
```
Error in library(testthat) : there is no package called ‘testthat’
```
Reason: Package `testthat` is not installed.

****Error 2****:
```
Error: could not find function "test_dir"
```
Reason: Package `testthat` is not loaded, run `library(testthat)`.

****Error 3****:
```
Failed ----------------------------------------------------------------------------------------------------------------------------
1. Failure: Testing New Tax Rate #2 (@test-numeric.R#71) --------------------------------------------------------------------------
result_ATP == 2.9 isn't true.
```
Reason: Changes to the calculator result in unexpected results and need to be reviewed. 

