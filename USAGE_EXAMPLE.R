# Example usage of the table1 package
# After installing, load the package:
# library(table1)

# Create example data
set.seed(123)
data <- data.frame(
  age = rnorm(100, 50, 10),
  sex = factor(rep(c("M", "F"), 50)),
  treated = rep(c(TRUE, FALSE), 50),
  score = runif(100, 0, 100),
  race = factor(rep(c("White", "Black", "Asian"), length.out = 100))
)

# Example 1: Simple usage
cat("Example 1: Simple usage\n")
cat("========================\n")
table1_simple <- specify_table1(data, vars = c("age", "sex", "treated"))
print(table1_simple)
cat("\n\n")

# Example 2: With custom labels
cat("Example 2: With custom labels\n")
cat("==============================\n")
varlist <- data.frame(
  var = c("age", "sex", "treated", "score"),
  label = c("Age (years)", "Sex", "Treated", "Score")
)
table1_labels <- specify_table1(data, vars = varlist)
print(table1_labels)
cat("\n\n")

# Example 3: With median and IQR
cat("Example 3: With median and IQR\n")
cat("==============================\n")
table1_median <- specify_table1(
  data,
  vars = c("age", "score"),
  center_fun = median,
  spread_fun = IQR
)
print(table1_median)
cat("\n\n")

# Example 4: With subheaders (data frame method)
cat("Example 4: With subheaders\n")
cat("==========================\n")
varlist_sub <- data.frame(
  var = c("**Demographics**", "age", "sex", "**Treatment**", "treated"),
  label = c("Demographics", "Age (years)", "Sex", "Treatment", "Treated")
)
table1_sub <- specify_table1(data, vars = varlist_sub)
print(table1_sub)
cat("\n\n")

# Example 5: With nested list
cat("Example 5: With nested list\n")
cat("============================\n")
varlist_nested <- list(
  "Patient Characteristics" = list(
    "Demographics" = list(
      age = "Age (years)",
      sex = "Sex"
    ),
    "Treatment" = list(
      treated = "Treated"
    )
  )
)
table1_nested <- specify_table1(data, vars = varlist_nested)
print(table1_nested)
cat("\n\n")

# Example 6: With variable-level function overrides
cat("Example 6: With variable-level overrides\n")
cat("=========================================\n")
varlist_override <- list(
  "Variables" = list(
    age = "Age (years)",
    score = list(
      var = "score",
      label = "Score",
      center_fun = median,
      spread_fun = IQR
    )
  )
)
table1_override <- specify_table1(data, vars = varlist_override)
print(table1_override)
cat("\n\n")

# Example 7: Multi-column table
cat("Example 7: Multi-column table\n")
cat("==============================\n")
data$group <- rep(c("A", "B"), 50)
table1_multi <- specify_table1_multi(
  data,
  vars = c("age", "sex", "treated"),
  subgroups = list(Group = "group"),
  include_all = TRUE
)
print(table1_multi)
cat("\n\n")

# Example 8: Grouped table
cat("Example 8: Grouped table\n")
cat("========================\n")
table1_grouped <- specify_table1(
  data,
  vars = c("age", "sex", "treated"),
  group = "group"
)
print(table1_grouped)
