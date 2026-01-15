# Script to review package documentation
# Run this after installing the package

library(table1)

cat("=== Table1 Package Documentation Review ===\n\n")

cat("1. Package Overview:\n")
cat("   ?table1\n")
cat("   (Run this in R console to see package help)\n\n")

cat("2. Main Functions:\n")
cat("   ?make_table1          - Create Table 1\n")
cat("   ?make_table1_multi    - Create multi-column Table 1\n")
cat("   ?fmt                  - Format numbers\n")
cat("   ?parse_yaml_varlist   - Parse YAML specifications\n\n")

cat("3. To view help pages, run in R console:\n")
cat("   help(package = 'table1')           # List all help pages\n")
cat("   ?make_table1                      # View function help\n")
cat("   example(make_table1)              # See examples\n\n")

cat("4. List all exported functions:\n")
cat("   ls('package:table1')\n\n")

cat("5. Package information:\n")
cat("   packageDescription('table1')\n")
cat("   packageVersion('table1')\n\n")

cat("=== Quick Test ===\n")
cat("Try running this to test the package:\n\n")
cat("data <- data.frame(\n")
cat("  age = rnorm(100, 50, 10),\n")
cat("  sex = factor(rep(c('M', 'F'), 50)),\n")
cat("  treated = rep(c(TRUE, FALSE), 50)\n")
cat(")\n")
cat("table1 <- make_table1(data, vars = c('age', 'sex', 'treated'))\n")
cat("print(table1)\n")
