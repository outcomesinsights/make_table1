#' Parse YAML Variable List Specification
#'
#' Parses a YAML string or file into the nested list structure used by make_table1.
#' Makes it easier for users to specify variables, labels, subheaders, and function overrides.
#'
#' @param yaml_input Character string with YAML content, or path to a YAML file
#' @param file Logical, if TRUE treats yaml_input as a file path (default = FALSE)
#'
#' @return Nested list structure compatible with make_table1
#'
#' @examples
#' \dontrun{
#' # YAML string
#' yaml_str <- "
#' Patient Characteristics:
#'   Demographics:
#'     age: Age (years)
#'     sex: Sex
#'   Treatment:
#'     treated: Treated
#' "
#' varlist <- parse_yaml_varlist(yaml_str)
#' table1 <- make_table1(data, vars = varlist)
#'
#' # YAML file
#' varlist <- parse_yaml_varlist("table1_spec.yaml", file = TRUE)
#' }
#'
#' @export
parse_yaml_varlist <- function(yaml_input, file = FALSE) {
  # Check if yaml package is available
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("The 'yaml' package is required for YAML parsing. ",
         "Install it with: install.packages('yaml')")
  }
  
  # Read YAML
  if (file) {
    if (!file.exists(yaml_input)) {
      stop("YAML file not found: ", yaml_input)
    }
    yaml_content <- yaml::read_yaml(yaml_input)
  } else {
    yaml_content <- yaml::read_yaml(text = yaml_input)
  }
  
  # Convert YAML structure to nested list format
  # YAML already produces nested lists, but we need to convert variable specifications
  converted <- .convert_yaml_to_varlist(yaml_content)
  
  return(converted)
}

#' Convert YAML Structure to Variable List Format
#'
#' Internal function to convert YAML-parsed structure to the format expected by make_table1.
#' Handles conversion of variable specifications (simple strings vs structured lists).
#'
#' @param yaml_list The parsed YAML structure
#'
#' @return Nested list in the format expected by make_table1
#'
#' @keywords internal
.convert_yaml_to_varlist <- function(yaml_list) {
  if (!is.list(yaml_list)) {
    return(yaml_list)
  }
  
  result <- list()
  
  for (i in seq_along(yaml_list)) {
    name <- names(yaml_list)[i]
    value <- yaml_list[[i]]
    
    if (is.null(name) || name == "") {
      # Unnamed element - pass through
      result[[i]] <- .convert_yaml_to_varlist(value)
    } else if (is.character(value) && length(value) == 1) {
      # Simple variable: name = "label"
      result[[name]] <- value
    } else if (is.list(value)) {
      # Check if this is a structured variable definition
      if ("var" %in% names(value) || "label" %in% names(value)) {
        # Structured variable - ensure it has the right format
        structured_var <- list()
        if ("var" %in% names(value)) {
          structured_var$var <- value$var
        } else {
          structured_var$var <- name  # Use name as variable if not specified
        }
        if ("label" %in% names(value)) {
          structured_var$label <- value$label
        } else {
          structured_var$label <- name
        }
        if ("center_fun" %in% names(value)) {
          # Convert function name string to function
          structured_var$center_fun <- .parse_function(value$center_fun)
        }
        if ("spread_fun" %in% names(value)) {
          structured_var$spread_fun <- .parse_function(value$spread_fun)
        }
        # Handle level specifications
        if ("levels" %in% names(value)) {
          structured_var$levels <- .parse_level_spec(value$levels)
        }
        result[[name]] <- structured_var
      } else {
        # Nested structure (subheader) - recurse
        result[[name]] <- .convert_yaml_to_varlist(value)
      }
    } else {
      # Other types - pass through
      result[[name]] <- value
    }
  }
  
  return(result)
}

#' Parse Function Name String to Function
#'
#' Internal function to convert function name strings (like "median", "mean") to actual functions.
#'
#' @param func_name Character string with function name
#'
#' @return Function object
#'
#' @keywords internal
.parse_function <- function(func_name) {
  if (is.function(func_name)) {
    return(func_name)
  }
  
  if (is.character(func_name) && length(func_name) == 1) {
    # Try to get function from base R or stats
    func <- tryCatch(
      get(func_name, envir = baseenv()),
      error = function(e) {
        tryCatch(
          get(func_name, envir = asNamespace("stats")),
          error = function(e2) {
            stop("Function '", func_name, "' not found")
          }
        )
      }
    )
    
    if (is.function(func)) {
      return(func)
    } else {
      stop("'", func_name, "' is not a function")
    }
  }
  
  stop("Invalid function specification: ", func_name)
}

#' Parse Level Specifications from YAML
#'
#' Internal function to convert level specifications from YAML format.
#' Handles conversion of function strings to actual R functions.
#' Supports both `function(x)` and `\(x)` syntax (R 4.1.0+).
#'
#' @param levels_list List of level specifications from YAML
#'
#' @return List with functions converted from strings
#'
#' @keywords internal
.parse_level_spec <- function(levels_list) {
  if (!is.list(levels_list)) {
    return(levels_list)
  }
  
  result <- list()
  
  for (i in seq_along(levels_list)) {
    level_name <- names(levels_list)[i]
    level_value <- levels_list[[i]]
    
    # Check if this is a function string
    if (is.character(level_value) && length(level_value) == 1) {
      # Check if it's a function string - supports both function(x) and \(x) syntax
      # Pattern matches: function(x) or \(x) at the start
      if (grepl("^(function\\s*\\(|\\\\\\()", level_value)) {
        # Parse and evaluate the function string
        tryCatch({
          func_expr <- parse(text = level_value)
          result[[level_name]] <- eval(func_expr)
        }, error = function(e) {
          stop("Could not parse function string for level '", level_name, "': ", level_value, "\nError: ", e$message)
        })
      } else {
        # Regular value, not a function
        result[[level_name]] <- level_value
      }
    } else {
      # Not a character string - pass through (could be numeric, etc.)
      result[[level_name]] <- level_value
    }
  }
  
  return(result)
}
