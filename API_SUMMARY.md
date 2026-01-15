# Table1 Package API Summary

## How to Specify Variables

### Current Implementation

The package supports multiple ways to specify variables and labels:

1. **Simple character vector**: `vars = c("age", "sex", "treated")`
2. **Character vector with labels**: Use `labels` parameter
3. **Two-column data frame**: `vars = data.frame(var = ..., label = ...)`
4. **Subheaders**: Include variable names that don't exist in data

### Subheaders

Subheaders are rows in the table that group related variables. They are created by:

1. **Including a variable name that doesn't exist in your data**:
   ```r
   varlist <- data.frame(
     var = c("**Demographics**", "age", "sex"),  # First one doesn't exist
     label = c("Demographics", "Age", "Sex")
   )
   ```

2. **Or creating a variable that is all NA** (matches original workflow):
   ```r
   # In your data preparation
   data$`**Demographics**` <- NA
   # Then include it in vars
   ```

## Current API

### `make_table1()`

```r
make_table1(
  data,                    # Data frame
  vars,                    # Variable names or 2-col data frame
  labels = NULL,           # Optional labels
  digits = 2,              # Decimal places
  center_fun = mean,       # Center statistic function
  spread_fun = sd,         # Spread statistic function
  group = NULL,            # Grouping variable for SMD
  var_types = NULL         # Override type detection
)
```

### `make_table1_multi()`

```r
make_table1_multi(
  data,                    # Data frame
  vars,                    # Variable names or 2-col data frame
  subgroups = NULL,        # Subgroup definitions
  include_all = TRUE,      # Include "all" column
  all_label = "All",       # Label for "all" column
  labels = NULL,           # Optional labels
  digits = 2,              # Decimal places
  center_fun = mean,       # Center statistic function
  spread_fun = sd,         # Spread statistic function
  var_types = NULL,         # Override type detection
  empty_subgroup_handling = "na"  # How to handle empty subgroups
)
```

## Questions for Review

1. **Is the subheader approach clear?** Should we add a helper function?
2. **Should we support a more structured input format?** (e.g., nested lists)
3. **Should subheaders be a separate parameter?** Or is the current approach fine?
4. **Do we need a function to prepare variable lists?** (like `prepare_varlist()`)

## Recommendations

1. **Add a helper function** `prepare_varlist()` to make it easier to create variable lists with subheaders
2. **Improve documentation** with more examples
3. **Add validation** to warn if subheader names look like they should be variables
4. **Consider a more structured format** for complex tables with many subheaders
