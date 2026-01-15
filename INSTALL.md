# Installing the table1 Package

## Installation Methods

### Method 1: Install from Local Directory (Recommended for Development)

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install the package from the local directory
devtools::install("/Users/mark/Documents/rprojects/package_development/table1")

# Or use the relative path from your current working directory
devtools::install("path/to/table1")
```

### Method 2: Install from Source

```r
# Navigate to the package directory in terminal, then:
R CMD build table1
R CMD INSTALL table1_0.0.0.9000.tar.gz

# Or in R:
install.packages("/path/to/table1_0.0.0.9000.tar.gz", repos = NULL, type = "source")
```

### Method 3: Load for Development (Without Installing)

```r
# Load devtools
library(devtools)

# Load the package directly (for development/testing)
load_all("/Users/mark/Documents/rprojects/package_development/table1")
```

## After Installation

```r
# Load the package
library(table1)

# Check that it's working
?make_table1
```

## Optional Dependencies

For YAML support, install the `yaml` package:

```r
install.packages("yaml")
```

The package will work without `yaml`, but YAML parsing features will not be available.

## Building Documentation

If you make changes to the code and need to regenerate documentation:

```r
library(roxygen2)
roxygenize("/Users/mark/Documents/rprojects/package_development/table1")
```

## Running Tests

After installation, run the test suite:

```r
library(devtools)
test("/Users/mark/Documents/rprojects/package_development/table1")
```

## Quick Start

```r
# Load the package
library(table1)

# Create some test data
data <- data.frame(
  age = rnorm(100, 50, 10),
  sex = factor(rep(c("M", "F"), 50)),
  treated = rep(c(TRUE, FALSE), 50)
)

# Create Table 1
table1 <- make_table1(data, vars = c("age", "sex", "treated"))
print(table1)
```
