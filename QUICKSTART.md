# Quick Start Guide

## Installation

```r
# Install devtools if needed
install.packages("devtools")

# Install the package
devtools::install("/Users/mark/Documents/rprojects/package_development/table1")

# Load the package
library(table1)
```

## Basic Usage

```r
# Create some data
data <- data.frame(
  age = rnorm(100, 50, 10),
  sex = factor(rep(c("M", "F"), 50)),
  treated = rep(c(TRUE, FALSE), 50)
)

# Create Table 1
table1 <- make_table1(data, vars = c("age", "sex", "treated"))
print(table1)
```

## With YAML (Recommended)

Create a file `table1_spec.yaml`:

```yaml
Patient Characteristics:
  Demographics:
    age: Age (years)
    sex: Sex
  Treatment:
    treated: Treated
```

Then use it:

```r
table1 <- make_table1(data, vars = "table1_spec.yaml")
```

## Multi-Column Tables

```r
data$group <- rep(c("A", "B"), 50)

table1_multi <- make_table1_multi(
  data,
  vars = c("age", "sex", "treated"),
  subgroups = list(Group = "group"),
  include_all = TRUE
)
```

## Next Steps

- See [YAML_EXAMPLES.md](YAML_EXAMPLES.md) for YAML syntax
- See [VARIABLE_SPECIFICATION.md](VARIABLE_SPECIFICATION.md) for all input methods
- See [MULTI_COLUMN_TABLES.md](MULTI_COLUMN_TABLES.md) for multi-column tables
- Run `?make_table1` for full documentation
