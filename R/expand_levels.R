#' Expand Variable with Level Specifications
#'
#' Internal function to expand a categorical variable into multiple rows
#' based on level specifications. Handles factors (with/without labels),
#' character vectors, and custom level definitions.
#'
#' @param data Data frame
#' @param variable Character string name of variable
#' @param label Character string label for the variable
#' @param level_spec Optional level specification. Can be:
#'   \itemize{
#'     \item NULL: Auto-expand all levels (default behavior)
#'     \item Named list: Each element is a function that returns logical vector
#'       (e.g., list("Level1" = function(x) x == "value1"))
#'     \item Character vector: Level names to include (for factors) or create
#'     \item List with values: list("Label1" = "value1", "Label2" = "value2")
#'   }
#' @param combine_remaining Logical, whether to add an "other" level for any
#'   values not included in level_spec (default = FALSE)
#' @param other_label Optional label for the "other" level (default = "Other")
#'
#' @return List of level definitions, each with:
#'   - label: Display label for the level
#'   - filter: Function that returns logical vector for that level
#'
#' @keywords internal
.expand_levels <- function(data, variable, label, level_spec = NULL,
                           combine_remaining = FALSE, other_label = NULL) {
  
  if (!variable %in% names(data)) {
    stop("Variable '", variable, "' not found in data")
  }
  
  x <- data[[variable]]
  
  # If no level specification, auto-expand based on variable type
  if (is.null(level_spec)) {
    return(.auto_expand_levels(x, label))
  }
  
  # Handle different level specification formats
  if (is.character(level_spec)) {
    # Character vector: level names
    level_defs <- .expand_levels_from_names(x, label, level_spec)
  } else if (is.list(level_spec)) {
    # Check if it's a named list of functions or a list of value mappings
    # Handle mixed lists (some functions, some values)
    has_functions <- any(sapply(level_spec, is.function))
    has_values <- any(!sapply(level_spec, is.function))
    
    if (has_functions && has_values) {
      # Mixed list - process separately
      func_list <- level_spec[sapply(level_spec, is.function)]
      value_list <- level_spec[!sapply(level_spec, is.function)]
      
      func_levels <- .expand_levels_from_functions(label, func_list)
      value_levels <- .expand_levels_from_values(x, label, value_list)
      
      # Combine both
      level_defs <- c(func_levels, value_levels)
    } else if (has_functions) {
      # All functions
      level_defs <- .expand_levels_from_functions(label, level_spec)
    } else {
      # All values
      level_defs <- .expand_levels_from_values(x, label, level_spec)
    }
  } else {
    stop("Invalid level_spec format for variable '", variable, "'")
  }
  
  # Add "other" level for remaining values if requested
  if (isTRUE(combine_remaining)) {
    if (is.null(other_label) || !nzchar(other_label)) {
      other_label <- "Other"
    }
    if (other_label %in% names(level_defs)) {
      new_label <- make.unique(c(names(level_defs), other_label))
      new_label <- new_label[length(new_label)]
      warning(
        "Level label '", other_label, "' already exists; using '", new_label, "' for remaining values."
      )
      other_label <- new_label
    }
    base_defs <- level_defs
    level_defs[[other_label]] <- local({
      defs <- base_defs
      function(d) {
        if (length(defs) == 0) {
          return(!is.na(d))
        }
        matches <- Reduce(
          `|`,
          lapply(defs, function(f) {
            res <- f(d)
            res[is.na(res)] <- FALSE
            res
          })
        )
        remaining <- !matches & !is.na(d)
        remaining
      }
    })
  }
  
  return(level_defs)
}

#' Auto-expand Levels Based on Variable Type
#'
#' Internal function to automatically expand levels for factors and categorical variables.
#'
#' @param x Variable vector
#' @param label Variable label
#'
#' @return List of level definitions
#'
#' @keywords internal
.auto_expand_levels <- function(x, label) {
  levels_list <- list()
  
  # Handle factors
  if (is.factor(x)) {
    levels_x <- levels(x)
    # Check for value labels (using haven package convention or similar)
    # For now, use level names directly - labels can be specified in level_spec
    for (level in levels_x) {
      level_label <- level  # Use level name as label by default
      
      # Create filter function for this level
      filter_fun <- local({
        lv <- level
        function(d) {
          # Convert to atomic
          if (is.factor(d)) {
            d_vals <- as.character(d)
            lv_comp <- as.character(lv)
          } else {
            d_vals <- c(d)
            lv_comp <- c(lv)
          }
          result <- d_vals == lv_comp
          result[is.na(d_vals)] <- FALSE
          result
        }
      })
      
      levels_list[[level_label]] <- filter_fun
    }
    return(levels_list)
  }
  
  # Handle character vectors - get unique values
  if (is.character(x)) {
    unique_vals <- unique(x[!is.na(x)])
    if (length(unique_vals) <= 20) {  # Reasonable number of levels
      for (val in unique_vals) {
        filter_fun <- local({
          v <- val
          function(d) {
            # Convert to atomic
            if (is.factor(d)) {
              d_vals <- as.character(d)
              v_comp <- as.character(v)
            } else {
              d_vals <- c(d)
              v_comp <- c(v)
            }
            result <- d_vals == v_comp
            result[is.na(d_vals)] <- FALSE
            result
          }
        })
        levels_list[[val]] <- filter_fun
      }
      return(levels_list)
    }
  }
  
  # If we can't auto-expand, return empty list (will be treated as single variable)
  return(list())
}

#' Expand Levels from Character Vector of Names
#'
#' Internal function to expand levels when given a character vector of level names.
#'
#' @param x Variable vector
#' @param label Variable label
#' @param level_names Character vector of level names
#'
#' @return List of level definitions
#'
#' @keywords internal
.expand_levels_from_names <- function(x, label, level_names) {
  levels_list <- list()
  
  # For factors, use the specified level names
  if (is.factor(x)) {
    levels_x <- levels(x)
    for (level_name in level_names) {
      if (level_name %in% levels_x) {
        filter_fun <- local({
          lv <- level_name
          function(d) {
            # Extract atomic values
            if (is.factor(d)) {
              d_vals <- as.character(unlist(d))
              lv_comp <- as.character(lv)
            } else {
              d_vals <- unlist(as.vector(d))
              lv_comp <- unlist(as.vector(lv))
            }
            result <- d_vals == lv_comp
            result[is.na(d_vals)] <- FALSE
            result
          }
        })
        levels_list[[level_name]] <- filter_fun
      } else {
        warning("Level '", level_name, "' not found in factor levels for variable")
      }
    }
    return(levels_list)
  }
  
  # For character vectors, check if values exist
  unique_vals <- unique(x[!is.na(x)])
  for (level_name in level_names) {
    if (level_name %in% unique_vals) {
        filter_fun <- local({
          v <- level_name
          function(d) {
            # Convert to atomic
            if (is.factor(d)) {
              d_vals <- as.character(d)
              v_comp <- as.character(v)
            } else {
              d_vals <- c(d)
              v_comp <- c(v)
            }
            result <- d_vals == v_comp
            result[is.na(d_vals)] <- FALSE
            result
          }
        })
      levels_list[[level_name]] <- filter_fun
    } else {
      warning("Level '", level_name, "' not found in variable values")
    }
  }
  
  return(levels_list)
}

#' Expand Levels from Named List of Functions
#'
#' Internal function to expand levels when given a named list of filter functions.
#'
#' @param label Variable label
#' @param level_functions Named list where each element is a function
#'
#' @return List of level definitions
#'
#' @keywords internal
.expand_levels_from_functions <- function(label, level_functions) {
  levels_list <- list()
  
  for (level_label in names(level_functions)) {
    filter_fun <- level_functions[[level_label]]
    if (!is.function(filter_fun)) {
      stop("Level specification for '", level_label, "' must be a function")
    }
    levels_list[[level_label]] <- filter_fun
  }
  
  return(levels_list)
}

#' Expand Levels from Value Mappings
#'
#' Internal function to expand levels when given a list mapping labels to values.
#'
#' @param x Variable vector
#' @param label Variable label
#' @param value_map Named list where names are labels and values are the values to match
#'
#' @return List of level definitions
#'
#' @keywords internal
.expand_levels_from_values <- function(x, label, value_map) {
  levels_list <- list()
  
  for (level_label in names(value_map)) {
    value <- value_map[[level_label]]
    
    # Create filter function
    filter_fun <- local({
      v <- value
      function(d) {
        # Convert d to atomic vector - handle all cases
        if (is.factor(d)) {
          d_vals <- as.character(d)
        } else if (inherits(d, "Date") || inherits(d, "POSIXt")) {
          d_vals <- as.numeric(d)
        } else {
          # Force to atomic - use c() to strip attributes
          d_vals <- c(d)
        }
        
        # Convert value to match
        if (is.factor(v)) {
          v_comp <- as.character(v)
        } else if (inherits(v, "Date") || inherits(v, "POSIXt")) {
          v_comp <- as.numeric(v)
        } else {
          v_comp <- c(v)
        }
        
        # Perform comparison - both should now be atomic
        result <- d_vals == v_comp
        result[is.na(d_vals)] <- FALSE
        result
      }
    })
    
    levels_list[[level_label]] <- filter_fun
  }
  
  return(levels_list)
}
