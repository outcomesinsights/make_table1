#' Create Table 1: Descriptive Statistics Summary
#'
#' Creates a Table 1 summary table that provides descriptive statistics
#' for variables in a data frame. Table 1 typically contains baseline
#' characteristics of study participants.
#'
#' The function automatically detects variable types and formats them appropriately:
#' \itemize{
#'   \item Continuous variables: Center (Spread) - default is Mean (SD)
#'   \item Binary variables: % (n)
#'   \item Categorical variables: % (n) for each level (auto-expanded or user-specified)
#' }
#' 
#' For categorical variables, you can specify which levels to include and their labels
#' using the \code{levels} parameter in nested list or YAML format. This eliminates the
#' need to manually create derived binary variables.
#' 
#' For factor variables specified without a \code{levels} specification, the function
#' automatically creates a subheader row with the variable label, followed by indented
#' rows for each factor level (indented with 2 spaces). This provides a clear hierarchical
#' structure in the table output.
#'
#' The function supports both single-column and multi-column tables. Use the \code{group}
#' parameter to create columns from a factor variable, or \code{subgroups} to define
#' custom columns (including non-mutually exclusive subsets).
#'
#' @param data Data frame or data table with variables of interest
#' @param vars Character vector of variable names, a data frame with 2 columns,
#'   a nested list structure, or a YAML string/file:
#'   \itemize{
#'     \item Character vector: Simple list of variable names
#'     \item Data frame: Column 1 = variable names, Column 2 = labels
#'     \item Nested list: Structured format with subheaders and variable-level overrides.
#'       See examples below.
#'   }
#'   
#'   Nested list format:
#'   \preformatted{
#'   list(
#'     "Table Title" = list(
#'       "Subheader 1" = list(
#'         var1 = "Label 1",
#'         var2 = "Label 2",
#'         var3 = list(var = "var3", label = "Label 3", 
#'                    center_fun = median, spread_fun = IQR),
#'         var4 = list(var = "var4", label = "Label 4",
#'                    levels = list("Level 1" = "value1", "Level 2" = "value2"))
#'       ),
#'       "Subheader 2" = list(...)
#'     )
#'   )
#'   }
#'   
#'   YAML format (recommended for readability):
#'   \preformatted{
#'   "Table Title":
#'     "Subheader 1":
#'       var1: "Label 1"
#'       var2: "Label 2"
#'       var3:
#'         var: "var3"
#'         label: "Label 3"
#'         center_fun: "median"
#'         spread_fun: "IQR"
#'       var4:
#'         var: "var4"
#'         label: "Label 4"
#'         levels:
#'           "Level 1": "value1"
#'           "Level 2": "value2"
#'     "Subheader 2":
#'       var5: "Label 5"
#'   }
#'   
#'   YAML can be provided as a string or file path. See \code{\link{parse_yaml_varlist}}
#'   for details.
#' @param labels Optional named vector or data frame mapping variable names to labels.
#'   If `vars` is a 2-column data frame, labels are taken from column 2.
#' @param digits Number of digits for continuous variables (default = 2)
#' @param center_fun Function for center statistic of continuous variables (default = `mean`).
#'   Other options: `median`, or custom function.
#' @param spread_fun Function for spread statistic of continuous variables (default = `sd`).
#'   Other options: `IQR` (for interquartile range), or custom function.
#' @param group Optional character string name of grouping variable. Creates columns
#'   for each level of the factor variable.
#' @param subgroups Optional named list defining custom subgroups/columns. Each element can be:
#'   \itemize{
#'     \item A character string naming a grouping variable (for mutually exclusive groups)
#'     \item A function that filters the data (returns logical vector)
#'     \item A list with 'filter' (function) and optional 'label' (character)
#'   }
#' @param include_all Logical, whether to include a column for all observations
#'   (default = TRUE if no group or subgroups specified, FALSE otherwise)
#' @param all_label Label for the "all" column (default = "All")
#' @param var_types Optional named character vector to override automatic type detection.
#'   Values should be "continuous", "binary", or "categorical".
#' @param empty_subgroup_handling How to handle empty subgroups:
#'   \itemize{
#'     \item "na" - Return NA for statistics (default)
#'     \item "zero" - Return "0" or "0 (0)" for statistics
#'     \item "skip" - Skip empty subgroups (not recommended for column alignment)
#'   }
#'
#' @return Returns Table 1 as a data frame with columns:
#'   \itemize{
#'     \item varname: Variable name/label
#'     \item One or more columns containing statistics (one per subgroup/group level)
#'     \item n: Sample size (for first column)
#'   }
#'   If multiple columns are created, the result includes an attribute "sample_sizes"
#'   with sample sizes for each column.
#'
#' @examples
#' # Create test data
#' data <- data.frame(
#'   age = rnorm(100, 50, 10),
#'   sex = factor(rep(c("M", "F"), 50)),
#'   treated = rep(c(TRUE, FALSE), 50),
#'   score = runif(100, 0, 100)
#' )
#'
#' # Simple usage with variable names (single column)
#' table1 <- specify_table1(data, vars = c("age", "sex", "treated", "score"))
#' print(table1)
#'
#' # With custom labels
#' varlist <- data.frame(
#'   var = c("age", "sex", "treated", "score"),
#'   label = c("Age (years)", "Sex", "Treated", "Score")
#' )
#' table1 <- specify_table1(data, vars = varlist)
#'
#' # Multi-column table by grouping variable
#' data$group <- rep(c("A", "B"), 50)
#' table1_multi <- specify_table1(
#'   data,
#'   vars = c("age", "sex", "treated"),
#'   group = "group"
#' )
#'
#' # Multi-column table with custom subgroups
#' table1_multi <- specify_table1(
#'   data,
#'   vars = c("age", "sex", "treated"),
#'   subgroups = list(
#'     "Treated" = function(d) d$treated == TRUE,
#'     "Untreated" = function(d) d$treated == FALSE
#'   ),
#'   include_all = TRUE
#' )
#'
#' # Factor variables automatically create subheader + indented levels
#' data$race <- factor(rep(c("White", "Black", "Asian"), c(30, 20, 50)))
#' table1_factor <- specify_table1(data, vars = c("age", "race"))
#' # Output: "race" as subheader, then "  White", "  Black", "  Asian" as indented rows
#'
#' @export
specify_table1 <- function(data, vars, labels = NULL, digits = 2,
                       center_fun = mean, spread_fun = sd,
                       group = NULL, subgroups = NULL,
                       include_all = NULL, all_label = "All",
                       var_types = NULL,
                       empty_subgroup_handling = c("na", "zero", "skip")) {
  
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  
  empty_subgroup_handling <- match.arg(empty_subgroup_handling)
  
  # Determine if we need multi-column table
  has_group <- !is.null(group)
  has_subgroups <- !is.null(subgroups) && length(subgroups) > 0
  
  # Set default for include_all
  if (is.null(include_all)) {
    include_all <- !has_group && !has_subgroups
  }
  
  # If no grouping specified and include_all is FALSE, default to single column
  if (!has_group && !has_subgroups && !include_all) {
    include_all <- TRUE
  }
  
  # Parse vars input - can be character vector, 2-column data frame, nested list, or YAML
  parsed_varlist <- NULL
  var_overrides <- list(center_funs = list(), spread_funs = list(), levels = list())
  
  # Check if vars is a YAML string or file path
  if (is.character(vars) && length(vars) == 1 && 
      (grepl("\n", vars) || grepl("^[A-Za-z_]", vars) && 
       (grepl(":", vars) || file.exists(vars)))) {
    # Likely YAML - try to parse it
    if (requireNamespace("yaml", quietly = TRUE)) {
      vars <- parse_yaml_varlist(vars, file = file.exists(vars))
    } else {
      warning("YAML input detected but 'yaml' package not available. ",
              "Install with: install.packages('yaml'). ",
              "Treating as character vector instead.")
    }
  }
  
  if (is.list(vars) && !is.data.frame(vars) && 
      (is.null(names(vars)) || any(sapply(vars, is.list)))) {
    # Nested list structure - parse it
    parsed_varlist <- .parse_varlist(vars)
    var_names <- parsed_varlist$vars$var
    var_labels <- parsed_varlist$vars$label
    var_subheaders <- parsed_varlist$vars$subheader
    # Ensure subheaders are never NA
    var_subheaders[is.na(var_subheaders)] <- ""
    var_overrides$center_funs <- parsed_varlist$center_funs
    var_overrides$spread_funs <- parsed_varlist$spread_funs
    var_overrides$levels <- parsed_varlist$levels
  } else if (is.data.frame(vars) && ncol(vars) >= 2) {
    # vars is a data frame with variable names and labels
    var_names <- vars[[1]]
    var_labels <- vars[[2]]
    var_subheaders <- if ("subheader" %in% names(vars)) {
      subh <- vars$subheader
      subh[is.na(subh)] <- ""
      subh
    } else {
      rep("", length(var_names))
    }
  } else if (is.character(vars)) {
    # vars is a character vector
    var_names <- vars
    var_labels <- if (!is.null(labels)) {
      if (is.data.frame(labels) && ncol(labels) >= 2) {
        # Match labels from data frame
        label_df <- labels
        var_labels <- label_df[[2]][match(var_names, label_df[[1]])]
        var_labels[is.na(var_labels)] <- var_names[is.na(var_labels)]
      } else if (is.vector(labels) && !is.null(names(labels))) {
        # Named vector
        labels[var_names]
      } else {
        # Use variable names as labels
        var_names
      }
    } else {
      var_names
    }
    var_subheaders <- rep("", length(var_names))
  } else {
    stop("'vars' must be a character vector, a 2-column data frame, or a nested list")
  }
  
  # Check for variables that should be factors and warn user
  .warn_about_missing_factors(data, var_names, var_overrides$levels, var_types)
  
  # If we have group or subgroups, create multi-column table
  if (has_group || has_subgroups || (include_all && (has_group || has_subgroups))) {
    # Convert group to subgroups format if needed
    if (has_group) {
      if (!group %in% names(data)) {
        stop("Grouping variable '", group, "' not found in data")
      }
      # Create subgroups list from group variable
      if (is.null(subgroups)) {
        subgroups <- list()
      }
      # Add group as a subgroup definition
      subgroups[[group]] <- group
    }
    
    # Process subgroups and create multi-column table
    return(.create_multi_column_table1(
      data = data,
      var_names = var_names,
      var_labels = var_labels,
      var_subheaders = var_subheaders,
      parsed_varlist = parsed_varlist,
      var_overrides = var_overrides,
      subgroups = subgroups,
      include_all = include_all,
      all_label = all_label,
      digits = digits,
      center_fun = center_fun,
      spread_fun = spread_fun,
      var_types = var_types,
      empty_subgroup_handling = empty_subgroup_handling
    ))
  } else {
    # Single column table - use core function
    return(.create_table1_core(
      data = data,
      var_names = var_names,
      var_labels = var_labels,
      var_subheaders = var_subheaders,
      var_overrides = var_overrides,
      digits = digits,
      center_fun = center_fun,
      spread_fun = spread_fun,
      var_types = var_types
    ))
  }
}

#' Check for variables that should be factors and warn user
#'
#' Identifies character/logical variables that are categorical but not factors,
#' and warns the user that they should be converted to factors for automatic
#' subheader + indented level display.
#'
#' @param data Data frame or data table
#' @param var_names Character vector of variable names to check
#' @param level_specs Named list of level specifications (variables with explicit levels are skipped)
#' @param var_types Optional named character vector of variable types
#'
#' @keywords internal
.warn_about_missing_factors <- function(data, var_names, level_specs, var_types = NULL) {
  missing_factors <- character()
  
  for (var_name in var_names) {
    # Skip if variable doesn't exist
    if (!var_name %in% names(data)) {
      next
    }
    
    # Skip if already a factor
    if (is.factor(data[[var_name]])) {
      next
    }
    
    # Skip if has explicit level specifications (user is handling it manually)
    if (!is.null(level_specs) && var_name %in% names(level_specs)) {
      next
    }
    
    # Get variable
    x <- data[[var_name]]
    
    # Only check character or logical variables
    if (!is.character(x) && !is.logical(x)) {
      next
    }
    
    # Detect type if not provided
    if (is.null(var_types) || !var_name %in% names(var_types)) {
      var_type <- .detect_type(data, var_name)
    } else {
      var_type <- var_types[[var_name]]
    }
    
    # Only warn if categorical (not continuous, binary, or other)
    if (var_type == "categorical") {
      missing_factors <- c(missing_factors, var_name)
    }
  }
  
  # Issue warning if any variables need conversion
  if (length(missing_factors) > 0) {
    if (length(missing_factors) == 1) {
      warning(
        "Variable '", missing_factors[1], "' is categorical but not a factor. ",
        "Consider converting it to a factor for automatic subheader + indented level display.\n",
        "  Example: data$", missing_factors[1], " <- factor(data$", missing_factors[1], ")",
        call. = FALSE
      )
    } else {
      var_list <- paste0("'", missing_factors, "'", collapse = ", ")
      warning(
        "The following variables are categorical but not factors: ", var_list, ".\n",
        "Consider converting them to factors for automatic subheader + indented level display.\n",
        "  Example: data$", missing_factors[1], " <- factor(data$", missing_factors[1], ")",
        call. = FALSE
      )
    }
  }
}

#' Internal function to create a single-column Table 1
#'
#' @keywords internal
.create_table1_core <- function(data, var_names, var_labels, var_subheaders,
                                var_overrides, digits, center_fun, spread_fun,
                                var_types) {
  
  # Process each variable
  results <- list()
  current_subheader <- ""
  
  for (i in seq_along(var_names)) {
    var_name <- var_names[i]
    var_label <- var_labels[i]
    var_subheader <- if (length(var_subheaders) >= i) {
      subh <- var_subheaders[i]
      if (is.na(subh) || is.null(subh)) "" else as.character(subh)
    } else {
      ""
    }
    
    # Ensure both subheaders are character strings (never NA)
    var_subheader <- if (is.na(var_subheader) || is.null(var_subheader)) "" else as.character(var_subheader)
    current_subheader <- if (is.na(current_subheader) || is.null(current_subheader)) "" else as.character(current_subheader)
    
    # Check if we need to add a subheader row (use identical for safe comparison)
    if (nchar(var_subheader) > 0 && !identical(var_subheader, current_subheader)) {
      # Add subheader row
      subheader_row <- data.frame(
        varname = var_subheader,
        statistic = NA_character_,
        n = NA_integer_,
        stringsAsFactors = FALSE
      )
      results[[length(results) + 1]] <- subheader_row
      current_subheader <- var_subheader
    }
    
    # Check if this is a subheader (variable doesn't exist in data)
    if (!var_name %in% names(data)) {
      # Create a subheader row (fallback for old-style subheaders)
      subheader_row <- data.frame(
        varname = var_label,
        statistic = NA_character_,
        n = NA_integer_,
        stringsAsFactors = FALSE
      )
      results[[length(results) + 1]] <- subheader_row
      next
    }
    
    # Get variable-specific function overrides
    var_center_fun <- if (var_name %in% names(var_overrides$center_funs)) {
      var_overrides$center_funs[[var_name]]
    } else {
      center_fun
    }
    
    var_spread_fun <- if (var_name %in% names(var_overrides$spread_funs)) {
      var_overrides$spread_funs[[var_name]]
    } else {
      spread_fun
    }
    
    # Get variable type override if provided
    var_type <- if (!is.null(var_types) && var_name %in% names(var_types)) {
      var_types[[var_name]]
    } else {
      NULL  # Auto-detect
    }
    
    # Get level specification if provided
    var_level_spec <- if (var_name %in% names(var_overrides$levels)) {
      var_overrides$levels[[var_name]]
    } else {
      NULL
    }
    
    # Summarize variable (no group parameter - SMD removed for now)
    # TODO: Re-implement SMD calculation in future version
    results[[length(results) + 1]] <- .summarize_variable(
      data = data,
      variable = var_name,
      label = var_label,
      var_type = var_type,
      digits = digits,
      center_fun = var_center_fun,
      spread_fun = var_spread_fun,
      group = NULL,  # SMD removed
      scaling = 100,
      level_spec = var_level_spec
    )
  }
  
  # Combine results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL
  
  return(result_df)
}

#' Internal function to create a multi-column Table 1
#'
#' @keywords internal
.create_multi_column_table1 <- function(data, var_names, var_labels, var_subheaders,
                                        parsed_varlist, var_overrides, subgroups,
                                        include_all, all_label, digits, center_fun,
                                        spread_fun, var_types, empty_subgroup_handling) {
  
  # Process subgroups
  subgroup_list <- list()
  subgroup_names <- character()
  
  # Add "all" column if requested
  if (include_all) {
    subgroup_list[["all"]] <- list(
      filter = function(d) rep(TRUE, nrow(d)),
      label = all_label
    )
    subgroup_names <- c(subgroup_names, all_label)
  }
  
  # Process provided subgroups
  if (!is.null(subgroups) && length(subgroups) > 0) {
    for (i in seq_along(subgroups)) {
      sub_name <- names(subgroups)[i]
      if (is.null(sub_name) || sub_name == "") {
        sub_name <- paste0("Subgroup_", i)
      }
      
      sub_def <- subgroups[[i]]
      
      # Handle different subgroup definition types
      if (is.character(sub_def) && length(sub_def) == 1) {
        # Grouping variable
        if (!sub_def %in% names(data)) {
          warning("Grouping variable '", sub_def, "' not found, skipping")
          next
        }
        
        group_var <- data[[sub_def]]
        group_levels <- unique(group_var[!is.na(group_var)])
        
        # Create a filter for each level
        # Use local() to properly capture loop variables
        for (level in group_levels) {
          level_label <- if (is.factor(level)) as.character(level) else level
          full_label <- paste(sub_name, level_label, sep = ": ")
          
          # Create closure with captured values using local()
          filter_fun <- local({
            gv_name <- sub_def
            lv_value <- level
            function(d) {
              d[[gv_name]] == lv_value & !is.na(d[[gv_name]])
            }
          })
          
          subgroup_list[[paste0(sub_name, "_", level_label)]] <- list(
            filter = filter_fun,
            label = full_label
          )
          subgroup_names <- c(subgroup_names, full_label)
        }
      } else if (is.function(sub_def)) {
        # Filter function
        subgroup_list[[sub_name]] <- list(
          filter = sub_def,
          label = sub_name
        )
        subgroup_names <- c(subgroup_names, sub_name)
      } else if (is.list(sub_def)) {
        # List with filter and optional label
        if (!"filter" %in% names(sub_def)) {
          stop("Subgroup definition must include 'filter' function")
        }
        sub_label <- if ("label" %in% names(sub_def)) {
          sub_def$label
        } else {
          sub_name
        }
        
        subgroup_list[[sub_name]] <- list(
          filter = sub_def$filter,
          label = sub_label
        )
        subgroup_names <- c(subgroup_names, sub_label)
      } else {
        warning("Unknown subgroup definition type for '", sub_name, "', skipping")
        next
      }
    }
  }
  
  # If no subgroups defined and include_all is FALSE, error
  if (length(subgroup_list) == 0) {
    stop("No subgroups defined. Provide 'subgroups' or set 'include_all = TRUE'")
  }
  
  # Create table for each subgroup
  subgroup_tables <- vector("list", length(subgroup_list))
  subgroup_ns <- integer(length(subgroup_list))
  
  for (i in seq_along(subgroup_list)) {
    sub_def <- subgroup_list[[i]]
    sub_label <- sub_def$label
    
    # Apply filter
    sub_filter <- tryCatch(
      sub_def$filter(data),
      error = function(e) {
        warning("Error applying filter for '", sub_label, "': ", e$message)
        return(rep(FALSE, nrow(data)))
      }
    )
    
    if (!is.logical(sub_filter) || length(sub_filter) != nrow(data)) {
      warning("Filter for '", sub_label, "' did not return logical vector of correct length, skipping")
      subgroup_tables[[i]] <- NULL
      subgroup_ns[i] <- 0L
      next
    }
    
    # Subset data
    sub_data <- data[sub_filter, , drop = FALSE]
    subgroup_ns[i] <- nrow(sub_data)
    
    # Handle empty subgroups
    if (nrow(sub_data) == 0) {
      if (empty_subgroup_handling == "skip") {
        subgroup_tables[[i]] <- NULL
        next
      }
      # Create placeholder - will be properly structured during alignment
      subgroup_tables[[i]] <- NULL
    } else {
      # Create table for this subgroup using core function
      if (!is.null(parsed_varlist)) {
        # Use nested list format if that's what was provided
        sub_table <- .create_table1_core(
          data = sub_data,
          var_names = var_names,
          var_labels = var_labels,
          var_subheaders = var_subheaders,
          var_overrides = var_overrides,
          digits = digits,
          center_fun = center_fun,
          spread_fun = spread_fun,
          var_types = var_types
        )
      } else {
        # Use data frame or character format
        sub_table <- .create_table1_core(
          data = sub_data,
          var_names = var_names,
          var_labels = var_labels,
          var_subheaders = var_subheaders,
          var_overrides = var_overrides,
          digits = digits,
          center_fun = center_fun,
          spread_fun = spread_fun,
          var_types = var_types
        )
      }
      
      # Handle empty subgroup statistics
      if (empty_subgroup_handling == "zero") {
        sub_table$statistic[is.na(sub_table$statistic)] <- "0"
        sub_table$statistic[sub_table$n == 0] <- "0 (0)"
      }
      
      subgroup_tables[[i]] <- sub_table
    }
  }
  
  # Remove skipped subgroups
  if (empty_subgroup_handling == "skip") {
    keep_idx <- !sapply(subgroup_tables, is.null)
    subgroup_tables <- subgroup_tables[keep_idx]
    subgroup_names <- subgroup_names[keep_idx]
    subgroup_ns <- subgroup_ns[keep_idx]
  }
  
  # Find reference table (first non-empty table)
  ref_idx <- which(!sapply(subgroup_tables, is.null))[1]
  if (is.na(ref_idx)) {
    stop("No non-empty subgroups found")
  }
  
  # Create empty structures for NULL tables based on reference
  ref_table <- subgroup_tables[[ref_idx]]
  for (i in seq_along(subgroup_tables)) {
    if (is.null(subgroup_tables[[i]])) {
      # Create empty structure matching reference
      empty_table <- ref_table
      empty_table$statistic <- if (empty_subgroup_handling == "zero") {
        "0 (0)"
      } else {
        NA_character_
      }
      empty_table$n <- 0L
      subgroup_tables[[i]] <- empty_table
    }
  }
  
  # Align all tables to have same structure
  aligned_tables <- .align_tables(subgroup_tables, reference_table = ref_idx)
  
  # Combine tables
  if (length(aligned_tables) == 0) {
    stop("No valid subgroups to combine")
  }
  
  # Start with first table (includes varname, statistic, n)
  result <- aligned_tables[[1]]
  
  # Rename statistic column to subgroup name
  names(result)[names(result) == "statistic"] <- subgroup_names[1]
  
  # Store n column for first subgroup (for per-cell n tracking)
  # Keep it as n_<subgroup_name> for consistency with other subgroups
  names(result)[names(result) == "n"] <- paste0("n_", subgroup_names[1])
  
  # Add remaining subgroups as additional columns
  if (length(aligned_tables) > 1) {
    for (i in 2:length(aligned_tables)) {
      sub_table <- aligned_tables[[i]]
      sub_name <- subgroup_names[i]
      
      # Add statistic column
      result[[sub_name]] <- sub_table$statistic
      
      # Add n column for per-cell n tracking (preserved for flextable formatting)
      result[[paste0("n_", sub_name)]] <- sub_table$n
    }
  }
  
  # Add attribute with sample sizes (column-level totals)
  attr(result, "sample_sizes") <- setNames(subgroup_ns, subgroup_names)
  
  return(result)
}
