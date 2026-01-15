# Quick script to load table1 for development
# Run this from R or RStudio

# Check if devtools is installed
if (!require("devtools", quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

# Set the package directory - UPDATE THIS PATH if needed
pkg_dir <- "/Users/mark/Documents/rprojects/package_development/table1"

# Verify the path exists
if (!dir.exists(pkg_dir)) {
  stop("Package directory not found: ", pkg_dir, 
       "\nPlease update pkg_dir in this script with the correct path.")
}

if (!file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
  stop("DESCRIPTION file not found in: ", pkg_dir,
       "\nThis doesn't appear to be a valid R package directory.")
}

cat("Loading table1 package from:", pkg_dir, "\n")
cat("(This is for development - changes will be reflected immediately)\n\n")

# Load the package
load_all(pkg_dir)

cat("Package loaded! You can now use:\n")
cat("  make_table1()\n")
cat("  make_table1_multi()\n")
cat("  fmt()\n")
cat("  parse_yaml_varlist()\n\n")
cat("After making code changes, run load_all() again to reload.\n")
