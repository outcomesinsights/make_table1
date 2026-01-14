#' Detect Variable Type
#'
#' Internal function to determine the type of a variable in a data frame.
#' Classifies variables as continuous, binary, categorical, or empty.
#' Improved version with better edge case handling.
#'
#' @param data Data frame or data table
#' @param variable Character string name of the variable
#'
#' @return Character string indicating type: "continuous", "binary", "categorical", "empty", or "other"
#'
#' @keywords internal
.detect_type <- function(data, variable) {
  # Check if variable exists
  if (!variable %in% names(data)) {
    stop("Variable '", variable, "' not found in data")
  }
  
  # Get the variable
  x <- data[[variable]]
  
  # Handle empty or all-NA
  n_valid <- sum(!is.na(x))
  if (n_valid == 0) {
    return("empty")
  }
  
  # Get class and unique values
  var_class <- class(x)[1]  # Use first class to handle multiple classes
  n_unique <- length(unique(x[!is.na(x)]))
  
  # Date/time types - treat as continuous
  if (var_class %in% c("Date", "POSIXct", "POSIXlt", "difftime")) {
    if (n_unique > 2) {
      return("continuous")
    } else {
      return("binary")
    }
  }
  
  # Factor handling
  if (var_class == "factor") {
    if (n_unique == 0) {
      return("empty")
    } else if (n_unique == 1) {
      return("binary")  # Single level factor
    } else if (n_unique == 2) {
      return("binary")
    } else {
      return("categorical")
    }
  }
  
  # Logical - always binary
  if (var_class == "logical") {
    return("binary")
  }
  
  # Character - treat as categorical if reasonable number of unique values
  if (var_class == "character") {
    if (n_unique == 1) {
      return("binary")
    } else if (n_unique <= 20 && n_unique < n_valid * 0.5) {
      # If relatively few unique values compared to sample size, treat as categorical
      return("categorical")
    } else {
      # Too many unique values - might be IDs or free text
      return("other")
    }
  }
  
  # Numeric types
  if (var_class %in% c("numeric", "integer", "double")) {
    if (n_unique == 0) {
      return("empty")
    } else if (n_unique == 1) {
      return("binary")  # Constant value
    } else if (n_unique == 2) {
      # Check if it's effectively binary (0/1 or similar)
      unique_vals <- sort(unique(x[!is.na(x)]))
      if (length(unique_vals) == 2 && 
          (all(unique_vals %in% c(0, 1)) || 
           all(unique_vals %in% c(0L, 1L)) ||
           all(unique_vals %in% c(FALSE, TRUE)))) {
        return("binary")
      } else {
        # Two distinct numeric values - could be continuous or binary
        # Default to continuous if range is large relative to values
        range_val <- diff(range(unique_vals, na.rm = TRUE))
        if (range_val > max(abs(unique_vals), na.rm = TRUE) * 0.1) {
          return("continuous")
        } else {
          return("binary")
        }
      }
    } else {
      # More than 2 unique values - continuous
      return("continuous")
    }
  }
  
  # Default: unknown type
  return("other")
}
