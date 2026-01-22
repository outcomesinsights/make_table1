#' Summarize a Single Variable
#'
#' Internal function to create a summary row for a single variable.
#' Handles different variable types and allows custom summary functions.
#'
#' @param data Data frame
#' @param variable Character string name of the variable
#' @param label Label for the variable (if NULL, uses variable name)
#' @param var_type Type of variable (auto-detected if NULL)
#' @param digits Number of digits for rounding
#' @param center_fun Function for center statistic (mean, median, etc.)
#' @param spread_fun Function for spread statistic (sd, IQR, etc.)
#' @param group Deprecated. Not currently used.
#' @param scaling Scaling factor for percentages (default = 100)
#' @param level_spec Optional level specification for categorical variables
#' @param combine_remaining Logical, whether to add an "other" level when
#'   level_spec is provided
#' @param other_label Optional label for the "other" level
#' @param binary_display Optional list controlling binary display (levels/value/layout)
#' @param n_pct_order Order for categorical stats: "n_pct" or "pct_n"
#'
#' @return Data frame with one or more rows (for categorical variables)
#'
#' @keywords internal
.format_n_pct <- function(pct_fmt, n_fmt, n_pct_order = c("n_pct", "pct_n")) {
  n_pct_order <- match.arg(n_pct_order)
  if (n_pct_order == "n_pct") {
    return(paste0(n_fmt, " (", pct_fmt, "%)"))
  }
  paste0(pct_fmt, "% (", n_fmt, ")")
}

.summarize_variable <- function(data, variable, label = NULL, var_type = NULL,
                                digits = 2, center_fun = mean, spread_fun = sd,
                                group = NULL, scaling = 100, level_spec = NULL,
                                combine_remaining = FALSE, other_label = NULL,
                                binary_display = NULL, n_pct_order = "n_pct",
                                center_fun_name = NULL, spread_fun_name = NULL) {
  
  # Get label
  if (is.null(label)) {
    label <- variable
  }
  
  # Get variable values
  x <- data[[variable]]
  n_valid <- sum(!is.na(x))
  
  # If level_spec is provided, treat as categorical regardless of detected type
  if (!is.null(level_spec)) {
    var_type <- "categorical"
  } else if (is.null(var_type)) {
    # Detect type if not provided and no level_spec
    var_type <- .detect_type(data, variable)
  }
  
  # Handle empty variables
  if (var_type == "empty" || n_valid == 0) {
    # Create subheader row with variable label
    subheader_row <- data.frame(
      varname = label,
      statistic = NA_character_,
      n = NA_integer_,
      stringsAsFactors = FALSE
    )
    
    # Create indented row with NA statistic
    indented_row <- data.frame(
      varname = paste0("\u00A0\u00A0", "N/A"),
      statistic = NA_character_,
      n = 0L,
      stringsAsFactors = FALSE
    )
    
    return(rbind(subheader_row, indented_row))
  }
  
  # The group parameter is now used to create multi-column tables instead
  
  # Continuous variables (but not if level_spec is provided - those are categorical)
  if (var_type == "continuous" && is.null(level_spec)) {
    center_val <- center_fun(x, na.rm = TRUE)
    spread_val <- spread_fun(x, na.rm = TRUE)
    
    # Format values
    center_fmt <- fmt(center_val, digits = digits)
    spread_fmt <- fmt(spread_val, digits = digits)
    
    statistic <- paste0(center_fmt, " (", spread_fmt, ")")
    
    # Create subheader row with variable label
    subheader_row <- data.frame(
      varname = label,
      statistic = NA_character_,
      n = NA_integer_,
      stringsAsFactors = FALSE
    )
    
    # Create indented row with statistic type label and actual statistic
    # Use non-breaking spaces (\u00A0) to ensure they display properly in Word/flextable
    # Get function names for labels
    center_name <- if (identical(center_fun, mean)) "Mean" else if (identical(center_fun, median)) "Median" else "Center"
    spread_name <- if (identical(spread_fun, sd)) "SD" else if (identical(spread_fun, IQR)) "IQR" else "Spread"
    
    stat_label <- paste0("\u00A0\u00A0", center_name, " (", spread_name, ")")
    indented_row <- data.frame(
      varname = stat_label,
      statistic = statistic,
      n = n_valid,
      stringsAsFactors = FALSE
    )
    
    return(rbind(subheader_row, indented_row))
  }
  
  # Binary variables
  if (var_type == "binary") {
    # Normalize binary display options
    binary_levels <- "single"
    binary_value <- "yes"
    binary_layout <- "one_row"
    
    if (is.list(binary_display)) {
      if (!is.null(binary_display$levels)) {
        binary_levels <- binary_display$levels
      }
      if (!is.null(binary_display$value)) {
        binary_value <- binary_display$value
      }
      if (!is.null(binary_display$layout)) {
        binary_layout <- binary_display$layout
      }
    }
    
    if (!binary_levels %in% c("single", "both")) {
      warning("Invalid binary_display$levels for variable '", variable, "'. Using 'single'.")
      binary_levels <- "single"
    }
    if (!binary_value %in% c("yes", "no")) {
      warning("Invalid binary_display$value for variable '", variable, "'. Using 'yes'.")
      binary_value <- "yes"
    }
    if (!binary_layout %in% c("one_row", "two_row")) {
      warning("Invalid binary_display$layout for variable '", variable, "'. Using 'one_row'.")
      binary_layout <- "one_row"
    }
    
    if (binary_levels == "both") {
      binary_layout <- "two_row"
    }
    
    # Helper to compute counts and labels
    get_binary_summary <- function(target) {
      if (is.factor(x)) {
        levels_x <- levels(x)
        yes_level <- if (length(levels_x) >= 2) levels_x[2] else levels_x[1]
        no_level <- levels_x[1]
        level_value <- if (target == "yes") yes_level else no_level
        level_label <- level_value
        n_target <- sum(x == level_value, na.rm = TRUE)
      } else if (is.logical(x)) {
        level_label <- if (target == "yes") "Yes" else "No"
        n_target <- if (target == "yes") sum(x, na.rm = TRUE) else sum(!x, na.rm = TRUE)
      } else {
        # Numeric binary (0/1)
        level_label <- if (target == "yes") "Yes" else "No"
        n_target <- if (target == "yes") {
          sum(x != 0 & !is.na(x))
        } else {
          sum(x == 0 & !is.na(x))
        }
      }
      
      pct <- (n_target / n_valid) * scaling
      pct_fmt <- fmt(pct, digits = digits)
      n_fmt <- fmt(n_target, digits = 0)
      
      list(
        label = level_label,
        statistic = .format_n_pct(pct_fmt, n_fmt, n_pct_order)
      )
    }
    
    if (binary_levels == "single") {
      summary <- get_binary_summary(binary_value)
      if (binary_layout == "one_row") {
        return(data.frame(
          varname = label,
          statistic = summary$statistic,
          n = n_valid,
          stringsAsFactors = FALSE
        ))
      }
      
      # Two-row layout: label row + indented level row
      subheader_row <- data.frame(
        varname = label,
        statistic = NA_character_,
        n = NA_integer_,
        stringsAsFactors = FALSE
      )
      
      indented_row <- data.frame(
        varname = paste0("\u00A0\u00A0", summary$label),
        statistic = summary$statistic,
        n = n_valid,
        stringsAsFactors = FALSE
      )
      
      return(rbind(subheader_row, indented_row))
    }
    
    # Both levels: label row + two indented rows
    subheader_row <- data.frame(
      varname = label,
      statistic = NA_character_,
      n = NA_integer_,
      stringsAsFactors = FALSE
    )
    
    if (is.factor(x)) {
      levels_x <- levels(x)
      level_values <- if (length(levels_x) >= 2) levels_x[1:2] else levels_x
      summaries <- lapply(level_values, function(level_value) {
        n_target <- sum(x == level_value, na.rm = TRUE)
        pct <- (n_target / n_valid) * scaling
        pct_fmt <- fmt(pct, digits = digits)
        n_fmt <- fmt(n_target, digits = 0)
        list(
          label = level_value,
          statistic = .format_n_pct(pct_fmt, n_fmt, n_pct_order)
        )
      })
    } else {
      summaries <- list(
        get_binary_summary("yes"),
        get_binary_summary("no")
      )
    }
    
    level_rows <- lapply(summaries, function(summary) {
      data.frame(
        varname = paste0("\u00A0\u00A0", summary$label),
        statistic = summary$statistic,
        n = n_valid,
        stringsAsFactors = FALSE
      )
    })
    
    return(rbind(subheader_row, do.call(rbind, level_rows)))
  }
  
  # Categorical variables (factors with >2 levels)
  # OR variables with level_spec provided (should be treated as categorical)
  if (var_type == "categorical" || !is.null(level_spec)) {
    # If level_spec is provided, use it; otherwise auto-expand
    if (!is.null(level_spec)) {
      # Expand levels using specification
      level_defs <- .expand_levels( # nolint
        data = data,
        variable = variable,
        label = label,
        level_spec = level_spec,
        combine_remaining = combine_remaining,
        other_label = other_label
      )
      
      if (length(level_defs) == 0) {
        # No levels to expand - treat as single variable with subheader + indented row
        warning("No valid levels found for variable '", variable, "' with level specification")
        
        # Create subheader row with variable label
        subheader_row <- data.frame(
          varname = label,
          statistic = NA_character_,
          n = NA_integer_,
          stringsAsFactors = FALSE
        )
        
        # Create indented row with NA statistic
        indented_row <- data.frame(
          varname = paste0("\u00A0\u00A0", "N/A"),
          statistic = NA_character_,
          n = n_valid,
          stringsAsFactors = FALSE
        )
        
        return(rbind(subheader_row, indented_row))
      }
      
      # Create subheader row with variable label
      subheader_row <- data.frame(
        varname = label,
        statistic = NA_character_,
        n = NA_integer_,
        stringsAsFactors = FALSE
      )
      
      result_rows <- vector("list", length(level_defs))
      
      for (i in seq_along(level_defs)) {
        level_label <- names(level_defs)[i]
        filter_fun <- level_defs[[i]]
        
        # Apply filter to create binary variable
        # x is already extracted from data[[variable]], so it should be a vector
        level_binary <- tryCatch({
          filter_fun(x)
        }, error = function(e) {
          # If filter fails, try converting x to atomic first
          x_atomic <- unlist(as.vector(x))
          filter_fun(x_atomic)
        })
        n_level <- sum(level_binary, na.rm = TRUE)
        pct <- (n_level / n_valid) * scaling
        
        pct_fmt <- fmt(pct, digits = digits)
        n_fmt <- fmt(n_level, digits = 0)
        
        # Indent level names with 2 spaces
        # Use non-breaking spaces (\u00A0) to ensure they display properly in Word/flextable
        indented_level_label <- paste0("\u00A0\u00A0", level_label)
        result_rows[[i]] <- data.frame(
          varname = indented_level_label,
          statistic = .format_n_pct(pct_fmt, n_fmt, n_pct_order),
          n = n_valid,
          stringsAsFactors = FALSE
        )
      }
      
      # Combine subheader + level rows
      return(rbind(subheader_row, do.call(rbind, result_rows)))
    } else {
      # Auto-expand: use current behavior
      # Check if variable is already a factor (before converting)
      is_factor_var <- is.factor(x)
      
      if (!is_factor_var) {
        x <- factor(x)
      }
      
      levels_x <- levels(x)
      n_levels <- length(levels_x)
      
      # For factor variables, create subheader + indented levels
      if (is_factor_var) {
        # Create subheader row with variable label
        subheader_row <- data.frame(
          varname = label,
          statistic = NA_character_,
          n = NA_integer_,
          stringsAsFactors = FALSE
        )
        
        # Create indented level rows
        level_rows <- vector("list", n_levels)
        
        for (i in seq_along(levels_x)) {
          level_i <- levels_x[i]
          n_level <- sum(x == level_i, na.rm = TRUE)
          pct <- (n_level / n_valid) * scaling
          
          pct_fmt <- fmt(pct, digits = digits)
          n_fmt <- fmt(n_level, digits = 0)
          
          # Indent level names with 2 spaces
          # Use non-breaking spaces (\u00A0) to ensure they display properly in Word/flextable
          # Regular spaces may be trimmed by some formatters
          level_label <- paste0("\u00A0\u00A0", level_i)
          level_rows[[i]] <- data.frame(
            varname = level_label,
            statistic = .format_n_pct(pct_fmt, n_fmt, n_pct_order),
            n = n_valid,
            stringsAsFactors = FALSE
          )
        }
        
        # Combine subheader + level rows
        return(rbind(subheader_row, do.call(rbind, level_rows)))
      } else {
        # Non-factor categorical (character): use same format as factors
        # Create subheader row with variable label
        subheader_row <- data.frame(
          varname = label,
          statistic = NA_character_,
          n = NA_integer_,
          stringsAsFactors = FALSE
        )
        
        # Create indented level rows
        level_rows <- vector("list", n_levels)
        
        for (i in seq_along(levels_x)) {
          level_i <- levels_x[i]
          n_level <- sum(x == level_i, na.rm = TRUE)
          pct <- (n_level / n_valid) * scaling
          
          pct_fmt <- fmt(pct, digits = digits)
          n_fmt <- fmt(n_level, digits = 0)
          
          # Indent level names with 2 spaces
          # Use non-breaking spaces (\u00A0) to ensure they display properly in Word/flextable
          level_label <- paste0("\u00A0\u00A0", level_i)
          level_rows[[i]] <- data.frame(
            varname = level_label,
            statistic = .format_n_pct(pct_fmt, n_fmt, n_pct_order),
            n = n_valid,
            stringsAsFactors = FALSE
          )
        }
        
        # Combine subheader + level rows
        return(rbind(subheader_row, do.call(rbind, level_rows)))
      }
    }
  }
  
  # Unknown type
  warning("Variable '", variable, "' has type '", var_type, 
          "' which is not fully supported. Attempting to treat as continuous.")
  
  center_val <- center_fun(x, na.rm = TRUE)
  spread_val <- spread_fun(x, na.rm = TRUE)
  
  center_fmt <- fmt(center_val, digits = digits)
  spread_fmt <- fmt(spread_val, digits = digits)
  
  statistic <- paste0(center_fmt, " (", spread_fmt, ")")
  
  return(data.frame(
    varname = label,
    statistic = statistic,
    n = n_valid,
    stringsAsFactors = FALSE
  ))
}
