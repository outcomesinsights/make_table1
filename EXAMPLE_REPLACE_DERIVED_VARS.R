# Example: Replacing Manual Derived Variables with Dynamic Level Specification
#
# This shows how the new level specification feature eliminates the need
# to manually create derived binary variables like:
#   dt_prep$age_18_44 <- dt_prep$age_group == "18-44"
#   dt_prep$age_45_54 <- dt_prep$age_group == "45-54"
# etc.

library(table1)

# Simulate the user's data structure
# In their actual script, they have:
# dt_prep$age_group (factor with levels: "18-44", "45-54", "55-64", "65-84", "85+")
# dt_prep$year (numeric: 2019, 2020, 2021)

# OLD APPROACH (what they currently do):
# dt_prep$age_18_44 <- dt_prep$age_group == "18-44"
# dt_prep$age_45_54 <- dt_prep$age_group == "45-54"
# dt_prep$age_55_64 <- dt_prep$age_group == "55-64"
# dt_prep$age_65_84 <- dt_prep$age_group == "65-84"
# dt_prep$age_85_plus <- dt_prep$age_group == "85+"
# dt_prep$year_2019 <- dt_prep$year == 2019
# dt_prep$year_2020 <- dt_prep$year == 2020
# dt_prep$year_2021 <- dt_prep$year == 2021
#
# Then in varlist:
# varlist <- data.frame(
#   var = c("age_18_44", "age_45_54", "age_55_64", "age_65_84", "age_85_plus", ...),
#   label = c("Age 18-44", "Age 45-54", "Age 55-64", "Age 65-84", "Age 85+", ...)
# )

# NEW APPROACH (using level specifications):
# No need to create derived variables! Just specify the levels directly:

# Option 1: Using nested list format
varlist_new <- list(
  "Patient Characteristics" = list(
    "Age at Diagnosis" = list(
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
      age = "Age (years)"  # Continuous variable, no levels needed
    ),
    "Year of Index Date" = list(
      year = list(
        var = "year",
        label = "Year of Index Date",
        levels = list(
          "2019" = 2019,
          "2020" = 2020,
          "2021" = 2021
        )
      )
    )
  )
)

# Option 2: Using YAML format (more readable for complex specifications)
yaml_spec <- "
Patient Characteristics:
  'Age at Diagnosis':
    age_group:
      var: age_group
      label: Age at Diagnosis
      levels:
        'Age 18-44': '18-44'
        'Age 45-54': '45-54'
        'Age 55-64': '55-64'
        'Age 65-84': '65-84'
        'Age 85+': '85+'
    age: 'Age (years)'
  'Year of Index Date':
    year:
      var: year
      label: Year of Index Date
      levels:
        '2019': 2019
        '2020': 2020
        '2021': 2021
"

# Usage:
# table1_result <- specify_table1(data = dt_prep, vars = varlist_new)
# OR
# table1_result <- specify_table1(data = dt_prep, vars = yaml_spec)

# For specify_table1_multi, it works the same way:
# table1_multi_result <- specify_table1_multi(
#   data = dt_prep,
#   vars = varlist_new,  # or yaml_spec
#   subgroups = subgroup_filters,
#   include_all = FALSE
# )

# Benefits:
# 1. No need to create derived variables manually
# 2. Cleaner data preparation code
# 3. Level labels can be customized independently of variable values
# 4. Works with factors (uses level names), character vectors, and numeric values
# 5. Can use custom filter functions for complex logic if needed
