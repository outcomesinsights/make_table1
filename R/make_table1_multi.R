#' Create Multi-Column Table 1 for Multiple Subgroups
#'
#' Creates a multi-column Table 1 that combines statistics from multiple
#' subgroups. Useful for comparing characteristics across different groups
#' or subsets of the data.
#'
#' Subgroups can be:
#' \itemize{
#'   \item Defined by a grouping variable (mutually exclusive groups)
#'   \item Defined by custom filters (non-mutually exclusive subsets)
#'   \item A combination of both
#' }
#'
#' @param data Data frame with variables of interest
#' @param vars Character vector of variable names, a 2-column data frame,
#'   a nested list structure, or a YAML string/file (see \code{\link{make_table1}})
#' @param subgroups Named list defining subgroups. Each element can be:
#'   \itemize{
#'     \item A character string naming a grouping variable (for mutually exclusive groups)
#'     \item A function that filters the data (returns logical vector)
#'     \item A list with 'filter' (function) and optional 'label' (character)
#'   }
#' @param include_all Logical, whether to include a column for all observations
#'   (default = TRUE)
#' @param all_label Label for the "all" column (default = "All")
#' @param labels Optional named vector mapping variable names to labels
#' @param digits Number of digits for continuous variables (default = 2)
#' @param center_fun Function for center statistic of continuous variables (default = `mean`)
#' @param spread_fun Function for spread statistic of continuous variables (default = `sd`)
#' @param var_types Optional named character vector to override automatic type detection
#' @param empty_subgroup_handling How to handle empty subgroups:
#'   \itemize{
#'     \item "na" - Return NA for statistics (default)
#'     \item "zero" - Return "0" or "0 (0)" for statistics
#'     \item "skip" - Skip empty subgroups (not recommended for column alignment)
#'   }
#'
#' @return Returns a data frame with columns:
#'   \itemize{
#'     \item varname: Variable name/label
#'     \item One column per subgroup containing the statistics
#'     \item n: Sample size (only for first column if include_all=TRUE)
#'   }
#'
#' @examples
#' # Create test data
#' data <- data.frame(
#'   age = rnorm(200, 50, 10),
#'   sex = factor(rep(c("M", "F"), 100)),
#'   treated = rep(c(TRUE, FALSE), 100),
#'   group = rep(c("A", "B", "C", "D"), 50)
#' )
#'
#' # Multi-column table by grouping variable
#' table1_multi <- make_table1_multi(
#'   data,
#'   vars = c("age", "sex", "treated"),
#'   subgroups = list(Group = "group")
#' )
#'
#' # Multi-column table with custom filters
#' table1_multi <- make_table1_multi(
#'   data,
#'   vars = c("age", "sex", "treated"),
#'   subgroups = list(
#'     "Treated" = function(d) d$treated == TRUE,
#'     "Untreated" = function(d) d$treated == FALSE,
#'     "Male" = function(d) d$sex == "M"
#'   )
#' )
#'
#' # Combine grouping variable and custom filters
#' table1_multi <- make_table1_multi(
#'   data,
#'   vars = c("age", "sex", "treated"),
#'   subgroups = list(
#'     "All Treated" = function(d) d$treated == TRUE,
#'     "Group A" = function(d) d$group == "A"
#'   ),
#'   include_all = TRUE
#' )
#'
#' @export
make_table1_multi <- function(data, vars, subgroups = NULL,
                             include_all = TRUE, all_label = "All",
                             labels = NULL, digits = 2,
                             center_fun = mean, spread_fun = sd,
                             var_types = NULL,
                             empty_subgroup_handling = c("na", "zero", "skip")) {
  
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  
  empty_subgroup_handling <- match.arg(empty_subgroup_handling)
  
  # Handle vars input - can be character vector, 2-column data frame, nested list, or YAML
  parsed_varlist <- NULL
  var_overrides <- list(center_funs = list(), spread_funs = list())
  
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
    var_overrides$center_funs <- parsed_varlist$center_funs
    var_overrides$spread_funs <- parsed_varlist$spread_funs
  } else if (is.data.frame(vars) && ncol(vars) >= 2) {
    # vars is a data frame with variable names and labels
    var_names <- vars[[1]]
    var_labels <- vars[[2]]
    var_subheaders <- if ("subheader" %in% names(vars)) vars$subheader else rep("", length(var_names))
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
  
  # Check for missing variables - these might be subheaders
  # (will be handled in make_table1, so we don't error here)
  
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
      # Create table for this subgroup
      # Use the same format as was provided (nested list, data frame, or character)
      if (!is.null(parsed_varlist)) {
        # Use nested list format if that's what was provided
        sub_table <- make_table1(
          data = sub_data,
          vars = vars,  # Pass original nested list
          labels = NULL,
          digits = digits,
          center_fun = center_fun,
          spread_fun = spread_fun,
          group = NULL,
          var_types = var_types
        )
      } else {
        # Use data frame or character format
        sub_table <- make_table1(
          data = sub_data,
          vars = data.frame(var = var_names, label = var_labels, stringsAsFactors = FALSE),
          labels = NULL,
          digits = digits,
          center_fun = center_fun,
          spread_fun = spread_fun,
          group = NULL,
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
  
  # Start with first table (includes varname, level, statistic, n)
  result <- aligned_tables[[1]]
  
  # Rename statistic column to subgroup name
  names(result)[names(result) == "statistic"] <- subgroup_names[1]
  
  # Add n column only for first subgroup (or "all" if included)
  if (!include_all || length(subgroup_names) > 1) {
    # Keep n column with descriptive name
    names(result)[names(result) == "n"] <- paste0("n_", subgroup_names[1])
  }
  
  # Add remaining subgroups as additional columns
  if (length(aligned_tables) > 1) {
    for (i in 2:length(aligned_tables)) {
      sub_table <- aligned_tables[[i]]
      sub_name <- subgroup_names[i]
      
      # Add statistic column
      result[[sub_name]] <- sub_table$statistic
      
      # Optionally add n column
      # (commented out to keep table narrower, but can be enabled)
      # result[[paste0("n_", sub_name)]] <- sub_table$n
    }
  }
  
  # Add attribute with sample sizes
  attr(result, "sample_sizes") <- setNames(subgroup_ns, subgroup_names)
  
  return(result)
}

