# table1

An R package for creating Table 1, which typically contains descriptive information about persons under study in statistical analyses. Table 1 provides summary statistics for baseline characteristics.

## Features

- **Automatic variable type detection** (continuous, binary, categorical)
- **Flexible summary statistics** (mean/median, SD/IQR, or custom functions)
- **Multi-column tables** for comparing subgroups
- **YAML support** for variable specification (optional)
- **Variable-level function overrides** for custom statistics

## Installation

### From Local Directory

```r
# Install devtools if needed
install.packages("devtools")

# Install from local directory
devtools::install("path/to/table1")
```

### For Development (Load Without Installing)

```r
library(devtools)
load_all("path/to/table1")
```

See [INSTALL.md](INSTALL.md) for detailed installation instructions.

## Quick Start

```r
library(table1)

# Create test data
data <- data.frame(
  age = rnorm(100, 50, 10),
  sex = factor(rep(c("M", "F"), 50)),
  treated = rep(c(TRUE, FALSE), 50),
  score = runif(100, 0, 100)
)

# Simple usage
table1 <- specify_table1(data, vars = c("age", "sex", "treated", "score"))
print(table1)

# With custom labels
varlist <- data.frame(
  var = c("age", "sex", "treated"),
  label = c("Age (years)", "Sex", "Treated")
)
table1 <- specify_table1(data, vars = varlist)

# With YAML (recommended for complex tables)
yaml_str <- "
Patient Characteristics:
  Demographics:
    age: Age (years)
    sex: Sex
  Treatment:
    treated: Treated
    score:
      var: score
      label: Score
      center_fun: median
      spread_fun: IQR
"
table1 <- specify_table1(data, vars = yaml_str)
```

## Documentation

- [YAML Examples](YAML_EXAMPLES.md) - How to use YAML for variable specification
- [Variable Specification](VARIABLE_SPECIFICATION.md) - All methods for specifying variables
- [Multi-Column Tables](MULTI_COLUMN_TABLES.md) - Creating tables with multiple subgroups
- [Installation Guide](INSTALL.md) - Detailed installation instructions

## Main Functions

- `specify_table1()` - Create Table 1 with optional subgroups
- `table1_to_flextable()` - Convert to `flextable` for Word output
- `fmt()` - Format numeric values with rounding and thousand separators
- `parse_yaml_varlist()` - Parse YAML specification for variables

## Tests

```r
devtools::test()
```

## Development

This package is under active development. See [REVIEW_CHECKLIST.md](REVIEW_CHECKLIST.md) and [ISSUES_TO_FIX.md](ISSUES_TO_FIX.md) for known issues and planned improvements.
