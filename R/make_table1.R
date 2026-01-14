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
#' @param vars Character vector of variable names, or a data frame with 2 columns:
#'   \itemize{
#'     \item Column 1: Variable names
#'     \item Column 2: Labels (optional, uses variable names if not provided)
#'   }
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
#'     \item level: Level (for categorical variables) or empty string
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
#' @export
make_table1 <- function(data, vars, labels = NULL, digits = 2,
                       center_fun = mean, spread_fun = sd,
                       group = NULL, var_types = NULL) {
  
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("'data' must be a data frame")
  }
  
  # Handle vars input - can be character vector or 2-column data frame
  if (is.data.frame(vars) && ncol(vars) >= 2) {
    # vars is a data frame with variable names and labels
    var_names <- vars[[1]]
    var_labels <- vars[[2]]
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
  } else {
    stop("'vars' must be a character vector or a 2-column data frame")
  }
  
  # Validate variables exist
  missing_vars <- var_names[!var_names %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }
  
  # Process each variable
  results <- vector("list", length(var_names))
  
  for (i in seq_along(var_names)) {
    var_name <- var_names[i]
    var_label <- var_labels[i]
    
    # Get variable type override if provided
    var_type <- if (!is.null(var_types) && var_name %in% names(var_types)) {
      var_types[[var_name]]
    } else {
      NULL  # Auto-detect
    }
    
    # Summarize variable
    results[[i]] <- .summarize_variable(
      data = data,
      variable = var_name,
      label = var_label,
      var_type = var_type,
      digits = digits,
      center_fun = center_fun,
      spread_fun = spread_fun,
      group = group,
      scaling = 100
    )
  }
  
  # Combine results
  result_df <- do.call(rbind, results)
  rownames(result_df) <- NULL
  
  return(result_df)
}
