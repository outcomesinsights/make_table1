# Nested List API for Variable Specification

## Overview

The nested list API provides a structured way to specify variables, labels, subheaders, and variable-level function overrides. This is the recommended approach for complex tables.

## Basic Structure

```r
list(
  "Table Title" = list(
    "Subheader 1" = list(
      var1 = "Label 1",
      var2 = "Label 2"
    ),
    "Subheader 2" = list(
      var3 = "Label 3"
    )
  )
)
```

## Examples

### Example 1: Simple Table with Subheaders

```r
data <- data.frame(
  age = rnorm(100, 50, 10),
  sex = factor(rep(c("M", "F"), 50)),
  treated = rep(c(TRUE, FALSE), 50),
  score = runif(100, 0, 100)
)

varlist <- list(
  "Patient Characteristics" = list(
    "Demographics" = list(
      age = "Age (years)",
      sex = "Sex"
    ),
    "Treatment" = list(
      treated = "Treated"
    ),
    "Outcomes" = list(
      score = "Score"
    )
  )
)

table1 <- specify_table1(data, vars = varlist)
```

### Example 2: With Variable-Level Function Overrides

Override center and spread functions for specific variables:

```r
varlist <- list(
  "Patient Characteristics" = list(
    "Demographics" = list(
      age = "Age (years)",  # Uses default mean and sd
      sex = "Sex"
    ),
    "Treatment" = list(
      treated = "Treated"
    ),
    "Outcomes" = list(
      score = list(
        var = "score",
        label = "Score",
        center_fun = median,
        spread_fun = IQR
      )
    )
  )
)

table1 <- specify_table1(data, vars = varlist)
```

### Example 3: Mixed Simple and Structured Variables

```r
varlist <- list(
  "Patient Characteristics" = list(
    "Demographics" = list(
      age = "Age (years)",  # Simple format
      sex = "Sex",          # Simple format
      bmi = list(           # Structured with overrides
        var = "bmi",
        label = "Body Mass Index",
        center_fun = median,
        spread_fun = IQR
      )
    ),
    "Treatment" = list(
      treated = "Treated"
    )
  )
)

table1 <- specify_table1(data, vars = varlist)
```

### Example 4: Without Table Title

You can omit the table title:

```r
varlist <- list(
  "Demographics" = list(
    age = "Age (years)",
    sex = "Sex"
  ),
  "Treatment" = list(
    treated = "Treated"
  )
)

table1 <- specify_table1(data, vars = varlist)
```

### Example 5: Complex Real-World Example

```r
varlist <- list(
  "Baseline Characteristics" = list(
    "Demographics" = list(
      age = "Age (years)",
      sex = "Sex",
      race = "Race"
    ),
    "Clinical Variables" = list(
      bmi = list(
        var = "bmi",
        label = "Body Mass Index (kg/m²)",
        center_fun = median,
        spread_fun = IQR
      ),
      sbp = list(
        var = "sbp",
        label = "Systolic Blood Pressure (mmHg)",
        center_fun = mean,
        spread_fun = sd
      )
    ),
    "Comorbidities" = list(
      diabetes = "Diabetes",
      hypertension = "Hypertension",
      ckd = "Chronic Kidney Disease"
    )
  )
)

table1 <- specify_table1(data, vars = varlist)
```

## Variable-Level Overrides

To override per-variable settings, use the structured format:

```r
variable_name = list(
  var = "variable_name",      # Required: variable name in data
  label = "Display Label",    # Required: label for table
  center_fun = median,        # Optional: override center function
  spread_fun = IQR,           # Optional: override spread function
  digits = 3,                 # Optional: override rounding
  levels = c("A", "B"),       # Optional: explicit categorical order
  combine_remaining = TRUE,   # Optional: add "other" bucket
  other_label = "Other",      # Optional: label for other bucket
  binary_display = list(      # Optional: binary display control
    levels = "single",        # "single" or "both"
    value = "yes",            # "yes" or "no" when single
    layout = "one_row"        # "one_row" or "two_row"
  )
)
```

Both `center_fun` and `spread_fun` are optional - you can specify one or both.

## Rules

1. **Table Title**: Optional top-level named element (single element list)
2. **Subheaders**: Named list elements that contain other variables
3. **Simple Variables**: `variable_name = "Label"` format
4. **Structured Variables**: `variable_name = list(var = ..., label = ..., ...)` format
5. **Function Overrides**: Only available in structured variable format

## Comparison with Other Methods

### Nested List (Recommended for Complex Tables)
```r
varlist <- list(
  "Demographics" = list(age = "Age", sex = "Sex")
)
```

### Data Frame Method
```r
varlist <- data.frame(
  var = c("age", "sex"),
  label = c("Age", "Sex"),
  subheader = c("Demographics", "Demographics")
)
```

### Character Vector Method
```r
vars <- c("age", "sex")
labels <- c(age = "Age", sex = "Sex")
```

## Benefits of Nested List API

1. **Clear structure** - Visual hierarchy matches table structure
2. **Variable-level overrides** - Easy to specify different functions per variable
3. **Type safety** - Structure enforces correct format
4. **Readability** - Easy to read and understand
5. **Flexibility** - Mix simple and structured formats
