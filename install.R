# Installation script for table1 package
# Run this from R or RStudio

# Check if devtools is installed
if (!require("devtools", quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

# Get the package directory
pkg_dir <- getwd()
if (basename(pkg_dir) != "table1") {
  # Try to find the package directory
  pkg_dir <- file.path(dirname(pkg_dir), "table1")
  if (!dir.exists(pkg_dir)) {
    stop("Could not find table1 package directory. Please set pkg_dir manually.")
  }
}

cat("Installing table1 package from:", pkg_dir, "\n")

# Install the package
install(pkg_dir)

cat("\nInstallation complete! Load the package with:\n")
cat("  library(table1)\n")
cat("\nTry it out:\n")
cat("  ?make_table1\n")
