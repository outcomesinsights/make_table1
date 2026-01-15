# Quick script to reload the updated table1 package
# Run this if you've updated the package but don't want to restart R

# Detach the package if it's loaded
if ("package:table1" %in% search()) {
  detach("package:table1", unload = TRUE)
}

# Reload the package
library(table1)

cat("Package reloaded! New version should now be active.\n")
cat("Version:", as.character(packageVersion("table1")), "\n")
