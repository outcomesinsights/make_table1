# Installation script for table1 package
# Run this from R or RStudio
# 
# Option 1: Run this script from within the package directory
# Option 2: Set pkg_dir manually to the full path

# Check if devtools is installed
if (!require("devtools", quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

# Get the package directory
# Try to auto-detect if we're in the package directory
pkg_dir <- getwd()
if (basename(pkg_dir) != "table1" || !file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
  # Try common location
  default_path <- "/Users/mark/Documents/rprojects/package_development/table1"
  if (dir.exists(default_path) && file.exists(file.path(default_path, "DESCRIPTION"))) {
    pkg_dir <- default_path
    cat("Using default path:", pkg_dir, "\n")
  } else {
    # Ask user to set manually
    cat("Could not auto-detect package directory.\n")
    cat("Please set pkg_dir manually. For example:\n")
    cat("  pkg_dir <- \"/Users/mark/Documents/rprojects/package_development/table1\"\n")
    cat("\nOr run this script from within the table1 package directory.\n")
    stop("Package directory not found. Please set pkg_dir manually.")
  }
}

cat("Installing table1 package from:", pkg_dir, "\n")

# Install the package
install(pkg_dir)

cat("\nInstallation complete! Load the package with:\n")
cat("  library(table1)\n")
cat("\nTry it out:\n")
cat("  ?specify_table1\n")
