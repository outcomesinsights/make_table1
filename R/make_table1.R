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
#'   \item Categorical variables: % (n) for each level
#' }
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
#'                    center_fun = median, spread_fun = IQR)
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
#'     "Subheader 2":
#'       var4: "Label 4"
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
#' @param group Optional character string name of grouping variable for calculating
#'   standardized mean differences (SMD) between two groups.
#' @param var_types Optional named character vector to override automatic type detection.
#'   Values should be "continuous", "binary", or "categorical".
#'
#' @return Returns Table 1 as a data frame with columns:
#'   \itemize{
#'     \item varname: Variable name/label
#'     \item statistic: Formatted statistic (Mean (SD) or % (n))
#'     \item n: Sample size
#'     \item (if group specified): group1_stat, group2_stat, smd
#'   }
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
#' # Simple usage with variable names
#' table1 <- make_table1(data, vars = c("age", "sex", "treated", "score"))
#' print(table1)
#'
#' # With custom labels
#' varlist <- data.frame(
#'   var = c("age", "sex", "treated", "score"),
#'   label = c("Age (years)", "Sex", "Treated", "Score")
#' )
#' table1 <- make_table1(data, vars = varlist)
#'
#' # With median and IQR instead of mean and SD
#' table1 <- make_table1(
#'   data, 
#'   vars = c("age", "score"),
#'   center_fun = median,
#'   spread_fun = IQR
#' )
#'
#' # With grouping for SMD calculation
#' data$group <- rep(c("A", "B"), 50)
#' table1_smd <- make_table1(
#'   data,
#'   vars = c("age", "sex", "treated"),
#'   group = "group"
#' )
#'
#' # With subheaders to group variables (data frame method)
#' varlist <- data.frame(
#'   var = c("**Demographics**", "age", "sex", "**Treatment**", "treated"),
#'   label = c("**Demographics**", "Age (years)", "Sex", "**Treatment**", "Treated")
#' )
#' table1 <- make_table1(data, vars = varlist)
#'
#' # With nested list structure (recommended for complex tables)
#' varlist <- list(
#'   "Patient Characteristics" = list(
#'     "Demographics" = list(
#'       age = "Age (years)",
#'       sex = "Sex"
#'     ),
#'     "Treatment" = list(
#'       treated = "Treated",
#'       score = list(var = "score", label = "Score", 
#'                   center_fun = median, spread_fun = IQR)
#'     )
#'   )
#' )
#' table1 <- make_table1(data, vars = varlist)
#'
#' # With YAML string (recommended for readability)
#' yaml_str <- "
#' Patient Characteristics:
#'   Demographics:
#'     age: Age (years)
#'     sex: Sex
#'   Treatment:
#'     treated: Treated
#'     score:
#'       var: score
#'       label: Score
#'       center_fun: median
#'       spread_fun: IQR
#' "
#' table1 <- make_table1(data, vars = yaml_str)
#'
#' # With YAML file
#' # writeLines(yaml_str, "table1_spec.yaml")
#' # table1 <- make_table1(data, vars = "table1_spec.yaml")
#'
#' @export
make_table1 <- function(data, vars, labels = NULL, digits = 2,
                       center_fun = mean, spread_fun = sd,
                       group = NULL, var_types = NULL) {
  
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  
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
    # table_title <- parsed_varlist$title  # Reserved for future use
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
  missing_vars <- var_names[!var_names %in% names(data)]
  if (length(missing_vars) > 0) {
    # Check if these are intended as subheaders (will be handled below)
    # If not, we'll error when we try to process them
  }
  
  # Process each variable
  results <- list()
  current_subheader <- NULL
  
  for (i in seq_along(var_names)) {
    var_name <- var_names[i]
    var_label <- var_labels[i]
    var_subheader <- if (length(var_subheaders) >= i) var_subheaders[i] else ""
    
    # Check if we need to add a subheader row
    if (var_subheader != "" && var_subheader != current_subheader) {
      # Add subheader row
      subheader_row <- data.frame(
        varname = var_subheader,
        statistic = NA_character_,
        n = NA_integer_,
        stringsAsFactors = FALSE
      )
      # Add group columns if needed
      if (!is.null(group)) {
        subheader_row$group1_stat <- NA_character_
        subheader_row$group2_stat <- NA_character_
        subheader_row$smd <- NA_character_
      }
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
      # Add group columns if needed
      if (!is.null(group)) {
        subheader_row$group1_stat <- NA_character_
        subheader_row$group2_stat <- NA_character_
        subheader_row$smd <- NA_character_
      }
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
    
    # Summarize variable
    results[[length(results) + 1]] <- .summarize_variable(
      data = data,
      variable = var_name,
      label = var_label,
      var_type = var_type,
      digits = digits,
      center_fun = var_center_fun,
      spread_fun = var_spread_fun,
      group = group,
      scaling = 100
    )
  }
  
  # Combine results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL
  
  return(result_df)
}
