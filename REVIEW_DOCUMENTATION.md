# Reviewing Package Documentation

The package is now installed! Here's how to access and review the documentation.

## Quick Access

### In R or RStudio Console

```r
# Load the package
library(table1)

# View package overview
help(package = "table1")

# View specific function documentation
?make_table1
?make_table1_multi
?fmt
?parse_yaml_varlist

# Browse all help pages
help.start()  # Opens HTML help browser
```

### Using the Helper Script

```r
source("/Users/mark/Documents/rprojects/package_development/table1/VIEW_DOCUMENTATION.R")
```

## Available Documentation

### Main Functions

1. **`make_table1()`** - Main function for creating Table 1
   - Access: `?make_table1`
   - Includes examples, parameter descriptions, return values

2. **`make_table1_multi()`** - Multi-column tables
   - Access: `?make_table1_multi`
   - Documentation for subgroup specifications

3. **`fmt()`** - Number formatting
   - Access: `?fmt`
   - Formatting options and examples

4. **`parse_yaml_varlist()`** - YAML parsing
   - Access: `?parse_yaml_varlist`
   - YAML syntax and examples

### Package Overview

- **`table1-package`** - Package overview
  - Access: `?table1-package` or `help(package = "table1")`
  - Overview of features and main functions

## Viewing in RStudio

1. **Help Pane** - Type `?make_table1` in console, help appears in Help pane
2. **F1 Key** - Place cursor on function name and press F1
3. **Help Tab** - Click Help tab, search for "table1"

## Viewing in Base R

1. **Console Help** - Type `?function_name` in console
2. **HTML Help** - Run `help.start()` to open browser-based help
3. **Package Index** - Run `help(package = "table1")` for index

## Documentation Files

The documentation is also available as `.Rd` files in the `man/` directory:
- `man/make_table1.Rd`
- `man/make_table1_multi.Rd`
- `man/fmt.Rd`
- `man/parse_yaml_varlist.Rd`
- `man/table1-package.Rd`

## Testing the Documentation

```r
library(table1)

# Test that help works
?make_table1

# Test package overview
help(package = "table1")

# List all exported functions
ls("package:table1")
```

## Rebuilding Documentation

If you make changes to the code and need to regenerate documentation:

```r
library(roxygen2)
roxygenize("/Users/mark/Documents/rprojects/package_development/table1")

# Then reinstall
library(devtools)
install("/Users/mark/Documents/rprojects/package_development/table1")
```

## Quick Test

```r
library(table1)

# View help
?make_table1

# Try the function
data <- data.frame(
  age = rnorm(100, 50, 10),
  sex = factor(rep(c("M", "F"), 50)),
  treated = rep(c(TRUE, FALSE), 50)
)

table1 <- make_table1(data, vars = c("age", "sex", "treated"))
print(table1)
```
