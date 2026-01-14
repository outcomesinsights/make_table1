# Multi-Column Table 1 Functionality

## Overview

The `make_table1_multi()` function creates multi-column Table 1 summaries that combine statistics from multiple subgroups. This is useful for comparing characteristics across different groups or subsets of your data.

## Key Features

- **Multiple subgroup types**: Support for grouping variables, custom filters, or combinations
- **Non-mutually exclusive groups**: Can include overlapping subgroups
- **Optional "All" column**: Automatically include a column for all observations
- **Empty subgroup handling**: Gracefully handles subgroups with 0 observations
- **Consistent structure**: All columns aligned to the same row structure

## Usage Examples

### Example 1: Grouping by a Variable

```r
# Create test data
data <- data.frame(
  age = rnorm(200, 50, 10),
  sex = factor(rep(c("M", "F"), 100)),
  treated = rep(c(TRUE, FALSE), 100),
  group = rep(c("A", "B", "C", "D"), 50)
)

# Multi-column table by grouping variable
table1_multi <- make_table1_multi(
  data,
  vars = c("age", "sex", "treated"),
  subgroups = list(Group = "group"),
  include_all = TRUE
)
```

This creates a table with columns for "All", "Group: A", "Group: B", "Group: C", and "Group: D".

### Example 2: Custom Filters (Non-Mutually Exclusive)

```r
# Multi-column table with custom filters
table1_multi <- make_table1_multi(
  data,
  vars = c("age", "sex", "treated"),
  subgroups = list(
    "Treated" = function(d) d$treated == TRUE,
    "Untreated" = function(d) d$treated == FALSE,
    "Male" = function(d) d$sex == "M",
    "Female" = function(d) d$sex == "F"
  ),
  include_all = TRUE
)
```

Note that "Treated" and "Male" can overlap - the function handles this correctly.

### Example 3: Combining Grouping Variable and Custom Filters

```r
table1_multi <- make_table1_multi(
  data,
  vars = c("age", "sex", "treated"),
  subgroups = list(
    "All Treated" = function(d) d$treated == TRUE,
    "Group A Only" = function(d) d$group == "A",
    "Treated in Group A" = function(d) d$treated == TRUE & d$group == "A"
  ),
  include_all = TRUE
)
```

### Example 4: Handling Empty Subgroups

```r
# Some subgroups might have 0 observations
table1_multi <- make_table1_multi(
  data,
  vars = c("age", "sex", "treated"),
  subgroups = list(
    "Group A" = function(d) d$group == "A",
    "Group Z" = function(d) d$group == "Z"  # Might not exist
  ),
  empty_subgroup_handling = "na"  # Options: "na", "zero", "skip"
)
```

## Parameters

### `subgroups`
A named list where each element defines a subgroup:

1. **Character string** (grouping variable name):
   ```r
   subgroups = list(Group = "group")
   ```
   Creates columns for each level of the grouping variable.

2. **Function** (custom filter):
   ```r
   subgroups = list("Treated" = function(d) d$treated == TRUE)
   ```
   The function should return a logical vector of length `nrow(data)`.

3. **List** (with filter and optional label):
   ```r
   subgroups = list(
     "Custom" = list(
       filter = function(d) d$age > 50,
       label = "Age > 50"
     )
   )
   ```

### `include_all`
Logical, whether to include a column for all observations (default = `TRUE`).

### `all_label`
Label for the "all" column (default = `"All"`).

### `empty_subgroup_handling`
How to handle empty subgroups:
- `"na"` (default): Return `NA` for statistics
- `"zero"`: Return `"0"` or `"0 (0)"` for statistics
- `"skip"`: Skip empty subgroups entirely (not recommended for column alignment)

## Output Structure

The output is a data frame with:
- `varname`: Variable name/label
- `level`: Level (for categorical variables) or empty string
- One column per subgroup containing the statistics
- `n`: Sample size (for first column, typically "All" if included)

The output also has an attribute `sample_sizes` containing the sample size for each subgroup:

```r
attr(table1_multi, "sample_sizes")
```

## Alignment and Structure

The function ensures all columns have the same structure:
- Same variables in the same order
- Same number of rows (important for categorical variables with multiple levels)
- Properly aligned even when subgroups have different numbers of observations

This is handled automatically by the `.align_tables()` internal function, which:
1. Uses the first non-empty subgroup as a reference structure
2. Aligns all other subgroups to match this structure
3. Fills in missing values appropriately for empty subgroups

## Comparison with Original Code

The original code in `table1_by_line.R`:
1. Created tables for each subgroup separately
2. Stored them in a list
3. Combined columns using `do.call(cbind, subtables)`
4. Required manual handling of empty subgroups

The new `make_table1_multi()` function:
- Automates the entire process
- Handles empty subgroups gracefully
- Ensures proper alignment automatically
- Supports flexible subgroup definitions
- Works with base R (no data.table dependency)
