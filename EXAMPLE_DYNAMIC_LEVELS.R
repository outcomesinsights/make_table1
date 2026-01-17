# Example: Dynamic Level Creation for Categorical Variables
# 
# This demonstrates how to use the new level specification feature to
# automatically create rows for categorical variable levels without
# manually creating derived binary variables.

library(table1)

# Create example data
data <- data.frame(
  age_group = factor(c("18-44", "45-54", "55-64", "65-84", "85+", 
                       "18-44", "45-54", "55-64", "65-84", "85+")),
  sex = factor(c("M", "F", "M", "F", "M", "F", "M", "F", "M", "F")),
  treated = c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
)

# Example 1: Auto-expand (current behavior - no level specification)
# This will create one row per level automatically
table1_auto <- specify_table1(
  data,
  vars = c("age_group", "sex", "treated")
)
print(table1_auto)

# Example 2: Specify which levels to include using value mapping
# This allows you to select specific levels and provide custom labels
varlist_levels <- list(
  "Patient Characteristics" = list(
    age_group = list(
      var = "age_group",
      label = "Age at Diagnosis",
      levels = list(
        "Age 18-44" = "18-44",
        "Age 45-54" = "45-54",
        "Age 55-64" = "55-64",
        "Age 65-84" = "65-84",
        "Age 85+" = "85+"
      )
    ),
    sex = "Sex",
    treated = "Treated"
  )
)

table1_specified <- specify_table1(data, vars = varlist_levels)
print(table1_specified)

# Example 3: Using YAML format (more readable)
yaml_spec <- "
Patient Characteristics:
  age_group:
    var: age_group
    label: Age at Diagnosis
    levels:
      'Age 18-44': '18-44'
      'Age 45-54': '45-54'
      'Age 55-64': '55-64'
      'Age 65-84': '65-84'
      'Age 85+': '85+'
  sex: Sex
  treated: Treated
"

table1_yaml <- specify_table1(data, vars = yaml_spec)
print(table1_yaml)

# Example 4: Using character vector to specify level names
# (useful when you want to use factor level names directly)
varlist_names <- list(
  "Patient Characteristics" = list(
    age_group = list(
      var = "age_group",
      label = "Age at Diagnosis",
      levels = c("18-44", "45-54", "55-64", "65-84", "85+")
    ),
    sex = "Sex"
  )
)

table1_names <- specify_table1(data, vars = varlist_names)
print(table1_names)

# Example 5: Using custom filter functions for complex logic
# (useful for character vectors or complex conditions)
data$year <- c(2019, 2019, 2020, 2020, 2021, 2019, 2020, 2021, 2019, 2020)

varlist_functions <- list(
  "Patient Characteristics" = list(
    year = list(
      var = "year",
      label = "Year of Index Date",
      levels = list(
        "2019" = function(x) x == 2019 & !is.na(x),
        "2020" = function(x) x == 2020 & !is.na(x),
        "2021" = function(x) x == 2021 & !is.na(x)
      )
    )
  )
)

table1_functions <- specify_table1(data, vars = varlist_functions)
print(table1_functions)
