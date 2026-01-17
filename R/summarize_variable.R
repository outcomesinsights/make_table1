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
#' @param group Optional grouping variable for SMD calculation
#' @param scaling Scaling factor for percentages (default = 100)
#' @param level_spec Optional level specification for categorical variables
#'
#' @return Data frame with one or more rows (for categorical variables)
#'
#' @keywords internal
.summarize_variable <- function(data, variable, label = NULL, var_type = NULL,
                                digits = 2, center_fun = mean, spread_fun = sd,
                                group = NULL, scaling = 100, level_spec = NULL) {
  
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
    return(data.frame(
      varname = label,
      statistic = NA_character_,
      n = 0L,
      stringsAsFactors = FALSE
    ))
  }
  
  # Handle grouping (SMD calculation)
  if (!is.null(group)) {
    return(.summarize_variable_grouped(
      data = data, variable = variable, label = label,
      var_type = var_type, digits = digits,
      center_fun = center_fun, spread_fun = spread_fun,
      group = group, scaling = scaling
    ))
  }
  
  # Continuous variables (but not if level_spec is provided - those are categorical)
  if (var_type == "continuous" && is.null(level_spec)) {
    center_val <- center_fun(x, na.rm = TRUE)
    spread_val <- spread_fun(x, na.rm = TRUE)
    
    # Format values
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
  
  # Binary variables
  if (var_type == "binary") {
    # Handle factors
    if (is.factor(x)) {
      # Use second level as reference
      if (nlevels(x) >= 2) {
        ref_level <- levels(x)[2]
        n_positive <- sum(x == ref_level, na.rm = TRUE)
      } else {
        ref_level <- levels(x)[1]
        n_positive <- sum(x == ref_level, na.rm = TRUE)
      }
    } else if (is.logical(x)) {
      n_positive <- sum(x, na.rm = TRUE)
      ref_level <- "TRUE"
    } else {
      # Numeric binary (0/1)
      n_positive <- sum(x != 0 & !is.na(x))
      ref_level <- "1"
    }
    
    pct <- (n_positive / n_valid) * scaling
    pct_fmt <- fmt(pct, digits = digits)
    n_fmt <- fmt(n_positive, digits = 0)
    
    statistic <- paste0(pct_fmt, "% (", n_fmt, ")")
    
    return(data.frame(
      varname = label,
      statistic = statistic,
      n = n_valid,
      stringsAsFactors = FALSE
    ))
  }
  
  # Categorical variables (factors with >2 levels)
  # OR variables with level_spec provided (should be treated as categorical)
  if (var_type == "categorical" || !is.null(level_spec)) {
    # If level_spec is provided, use it; otherwise auto-expand
    if (!is.null(level_spec)) {
      # Expand levels using specification
      level_defs <- .expand_levels(data, variable, label, level_spec)
      
      if (length(level_defs) == 0) {
        # No levels to expand - treat as single variable
        warning("No valid levels found for variable '", variable, "' with level specification")
        return(data.frame(
          varname = label,
          statistic = NA_character_,
          n = n_valid,
          stringsAsFactors = FALSE
        ))
      }
      
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
        
        result_rows[[i]] <- data.frame(
          varname = level_label,
          statistic = paste0(pct_fmt, "% (", n_fmt, ")"),
          n = n_valid,
          stringsAsFactors = FALSE
        )
      }
      
      return(do.call(rbind, result_rows))
    } else {
      # Auto-expand: use current behavior
      if (!is.factor(x)) {
        x <- factor(x)
      }
      
      levels_x <- levels(x)
      n_levels <- length(levels_x)
      
      result_rows <- vector("list", n_levels)
      
      for (i in seq_along(levels_x)) {
        level_i <- levels_x[i]
        n_level <- sum(x == level_i, na.rm = TRUE)
        pct <- (n_level / n_valid) * scaling
        
        pct_fmt <- fmt(pct, digits = digits)
        n_fmt <- fmt(n_level, digits = 0)
        
        # For categorical variables, create separate rows for each level
        # Use the level name as part of the varname for clarity
        level_label <- paste0(label, " - ", level_i)
        result_rows[[i]] <- data.frame(
          varname = level_label,
          statistic = paste0(pct_fmt, "% (", n_fmt, ")"),
          n = n_valid,
          stringsAsFactors = FALSE
        )
      }
      
      return(do.call(rbind, result_rows))
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

#' Summarize Variable with Grouping (for SMD)
#'
#' Internal function to summarize a variable by group and calculate SMD.
#'
#' @inheritParams .summarize_variable
#'
#' @return Data frame with group statistics and SMD
#'
#' @keywords internal
.summarize_variable_grouped <- function(data, variable, label, var_type,
                                       digits, center_fun, spread_fun,
                                       group, scaling) {
  
  # Check group variable
  if (!group %in% names(data)) {
    stop("Group variable '", group, "' not found in data")
  }
  
  group_var <- data[[group]]
  group_levels <- unique(group_var[!is.na(group_var)])
  
  if (length(group_levels) != 2) {
    stop("Group variable must have exactly 2 levels, found ", length(group_levels))
  }
  
  # Split data by group
  data_group1 <- data[group_var == group_levels[1], , drop = FALSE]
  data_group2 <- data[group_var == group_levels[2], , drop = FALSE]
  
  # Summarize for each group
  sum1 <- .summarize_variable(
    data = data_group1, variable = variable, label = label,
    var_type = var_type, digits = digits,
    center_fun = center_fun, spread_fun = spread_fun,
    group = NULL, scaling = scaling
  )
  
  sum2 <- .summarize_variable(
    data = data_group2, variable = variable, label = label,
    var_type = var_type, digits = digits,
    center_fun = center_fun, spread_fun = spread_fun,
    group = NULL, scaling = scaling
  )
  
  # Calculate SMD for continuous variables
  if (var_type == "continuous") {
    x1 <- data_group1[[variable]]
    x2 <- data_group2[[variable]]
    
    mean1 <- center_fun(x1, na.rm = TRUE)
    mean2 <- center_fun(x2, na.rm = TRUE)
    sd1 <- spread_fun(x1, na.rm = TRUE)
    sd2 <- spread_fun(x2, na.rm = TRUE)
    
    # Pooled SD for SMD
    pooled_sd <- sqrt((sd1^2 + sd2^2) / 2)
    smd <- (mean1 - mean2) / pooled_sd
    
    # Combine results
    result <- data.frame(
      varname = label,
      group1_stat = sum1$statistic,
      group2_stat = sum2$statistic,
      smd = fmt(smd, digits = digits),
      stringsAsFactors = FALSE
    )
    
    return(result)
  }
  
  # For categorical/binary, calculate SMD differently
  # This is a simplified version - could be improved
  if (var_type %in% c("binary", "categorical")) {
    x1 <- data_group1[[variable]]
    x2 <- data_group2[[variable]]
    
    # For binary, use proportion difference
    if (var_type == "binary") {
      if (is.factor(x1)) {
        p1 <- mean(x1 == levels(x1)[min(2, nlevels(x1))], na.rm = TRUE)
        p2 <- mean(x2 == levels(x2)[min(2, nlevels(x2))], na.rm = TRUE)
      } else {
        p1 <- mean(x1 != 0 & !is.na(x1), na.rm = TRUE)
        p2 <- mean(x2 != 0 & !is.na(x2), na.rm = TRUE)
      }
      
      pooled_p <- (p1 + p2) / 2
      pooled_se <- sqrt(pooled_p * (1 - pooled_p))
      smd <- (p1 - p2) / pooled_se
    } else {
      # For categorical, use first level proportion
      if (!is.factor(x1)) x1 <- factor(x1)
      if (!is.factor(x2)) x2 <- factor(x2)
      
      p1 <- mean(x1 == levels(x1)[1], na.rm = TRUE)
      p2 <- mean(x2 == levels(x2)[1], na.rm = TRUE)
      
      pooled_p <- (p1 + p2) / 2
      pooled_se <- sqrt(pooled_p * (1 - pooled_p))
      smd <- (p1 - p2) / pooled_se
    }
    
    # Combine results (simplified - would need to handle multiple levels)
    result <- data.frame(
      varname = label,
      group1_stat = sum1$statistic,
      group2_stat = sum2$statistic,
      smd = fmt(smd, digits = digits),
      stringsAsFactors = FALSE
    )
    
    return(result)
  }
  
  # Default: return without SMD
  result <- data.frame(
    varname = label,
    group1_stat = sum1$statistic,
    group2_stat = sum2$statistic,
    smd = NA_character_,
    stringsAsFactors = FALSE
  )
  
  return(result)
}
