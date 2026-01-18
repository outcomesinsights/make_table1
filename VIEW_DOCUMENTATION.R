# Quick script to view package documentation
# Run this after installing the package

# Load the package
library(table1)

cat("=== table1 Package Documentation ===\n\n")

cat("Main Functions:\n")
cat("  ?specify_table1          - Create Table 1 with optional group columns\n")
cat("  ?specify_table1_multi    - Create multi-column Table 1\n")
cat("  ?fmt                  - Format numeric values\n")
cat("  ?parse_yaml_varlist   - Parse YAML variable specifications\n\n")

cat("Package Overview:\n")
cat("  ?table1-package       - Package overview and features\n\n")

cat("To view any help page, use:\n")
cat("  ?function_name\n\n")

cat("Example:\n")
cat("  ?specify_table1\n\n")

cat("To see all exported functions:\n")
cat("  ls('package:table1')\n\n")

cat("To browse the package:\n")
cat("  help(package = 'table1')\n\n")

# Open the package help
help(package = "table1")
