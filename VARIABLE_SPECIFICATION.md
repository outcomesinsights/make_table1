# Specifying Variables and Labels for Table 1

## Overview

There are several ways to specify which variables to include in Table 1 and how to label them. You can also add subheaders to group related variables.

## Method 1: Simple Character Vector

The simplest approach - just list the variable names:

```r
table1 <- make_table1(data, vars = c("age", "sex", "treated", "score"))
```

Labels will be the same as variable names.

## Method 2: Character Vector with Named Labels

Provide labels using a named vector:

```r
table1 <- make_table1(
  data,
  vars = c("age", "sex", "treated", "score"),
  labels = c(
    age = "Age (years)",
    sex = "Sex",
    treated = "Treated",
    score = "Score"
  )
)
```

## Method 3: Two-Column Data Frame (Recommended)

Create a data frame with variable names and labels:

```r
varlist <- data.frame(
  var = c("age", "sex", "treated", "score"),
  label = c("Age (years)", "Sex", "Treated", "Score")
)

table1 <- make_table1(data, vars = varlist)
```

## Method 4: With Subheaders

To add subheaders that group related variables, include variable names that don't exist in your data. These will be treated as subheader rows:

```r
varlist <- data.frame(
  var = c(
    "**Demographics**",  # Subheader - doesn't exist in data
    "age",
    "sex",
    "**Treatment**",     # Subheader
    "treated",
    "**Outcomes**",      # Subheader
    "score"
  ),
  label = c(
    "**Demographics**",
    "Age (years)",
    "Sex",
    "**Treatment**",
    "Treated",
    "**Outcomes**",
    "Score"
  )
)

table1 <- make_table1(data, vars = varlist)
```

The subheader rows will appear in the table with the label text and no statistics.

## Method 5: Matching Original Workflow

If you want to match the original workflow from your analysis code, you can create a data frame with variables (including NA columns for subheaders) and then extract the names:

```r
# Create data frame with variables and subheaders
t1 <- data.frame(
  `**Age at Diagnosis**` = NA,  # Subheader
  `Age (years)` = data$age,
  `Age 66-69` = data$age_group == "66-69",
  `Age 70-74` = data$age_group == "70-74",
  # ... more variables
  check.names = FALSE
)

# Create variable list from column names
varlist <- data.frame(
  var = names(t1),
  label = names(t1)
)

table1 <- make_table1(data, vars = varlist)
```

Note: Variables that are all NA will be detected as subheaders automatically.

## Best Practices

1. **Use Method 3 or 4** for clarity and control over labels
2. **Use descriptive labels** - they appear in the final table
3. **Group related variables** with subheaders for better readability
4. **Keep variable names consistent** between your data and variable list

## Examples

### Example 1: Basic Table

```r
data <- data.frame(
  age = rnorm(100, 50, 10),
  sex = factor(rep(c("M", "F"), 50)),
  treated = rep(c(TRUE, FALSE), 50)
)

table1 <- make_table1(data, vars = c("age", "sex", "treated"))
```

### Example 2: With Custom Labels

```r
varlist <- data.frame(
  var = c("age", "sex", "treated"),
  label = c("Age (years)", "Sex", "Treated")
)

table1 <- make_table1(data, vars = varlist)
```

### Example 3: With Subheaders

```r
varlist <- data.frame(
  var = c(
    "**Demographics**",
    "age",
    "sex",
    "**Treatment Status**",
    "treated"
  ),
  label = c(
    "Demographics",
    "Age (years)",
    "Sex",
    "Treatment Status",
    "Treated"
  )
)

table1 <- make_table1(data, vars = varlist)
```

### Example 4: Complex Structure (Matching Original)

```r
# Create expanded data frame with all variables
t1 <- data.frame(
  `**Age at Diagnosis**` = NA,
  `Age (years)` = data$age,
  `Age 66-69` = data$age_group == "66-69",
  `Age 70-74` = data$age_group == "70-74",
  `**Sex and Race**` = NA,
  `Male` = data$sex == "M",
  `White` = data$race == "White",
  `Black` = data$race == "Black",
  check.names = FALSE
)

# Extract variable list
varlist <- data.frame(
  var = names(t1),
  label = names(t1)
)

table1 <- make_table1(data, vars = varlist)
```
