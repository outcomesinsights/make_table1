#' Parse Nested Variable List Structure
#'
#' Internal function to parse a nested list structure for variables.
#' Supports subheaders and variable-level overrides for summary functions.
#'
#' Structure:
#' list(
#'   "Table Title" = list(
#'     "Subheader 1" = list(
#'       var1 = "Label 1",
#'       var2 = "Label 2",
#'       var3 = list(var = "var3", label = "Label 3", center_fun = median, spread_fun = IQR)
#'     ),
#'     "Subheader 2" = list(...)
#'   )
#' )
#'
#' @param varlist Nested list structure
#'
#' @return List with:
#'   - vars: data frame with var, label, subheader columns
#'   - center_funs: named list of center function overrides
#'   - spread_funs: named list of spread function overrides
#'   - title: table title if provided
#'
#' @keywords internal
.parse_varlist <- function(varlist) {
  
  if (!is.list(varlist)) {
    stop("varlist must be a list")
  }
  
  # Storage for results
  var_names <- character()
  var_labels <- character()
  var_subheaders <- character()
  var_center_funs <- list()
  var_spread_funs <- list()
  table_title <- NULL
  
  # Check if top level has a title (single named element at top level)
  if (length(varlist) == 1 && !is.null(names(varlist)) && 
      is.list(varlist[[1]]) && length(varlist[[1]]) > 0) {
    # Likely a title - extract it
    table_title <- names(varlist)[1]
    varlist <- varlist[[1]]
  }
  
  # Recursive function to process nested structure
  .process_level <- function(level, current_subheader = NULL) {
    if (is.null(level) || length(level) == 0) {
      return()
    }
    
    for (i in seq_along(level)) {
      item_name <- names(level)[i]
      item <- level[[i]]
      
      if (is.list(item)) {
        # Check if it's a structured variable definition or a subheader
        if ("var" %in% names(item)) {
          # Structured variable definition with optional overrides
          var_names <<- c(var_names, item$var)
          var_labels <<- c(var_labels, if ("label" %in% names(item)) item$label else item$var)
          var_subheaders <<- c(var_subheaders, if (is.null(current_subheader)) "" else current_subheader)
          
          # Store function overrides if provided
          if ("center_fun" %in% names(item)) {
            var_center_funs[[item$var]] <<- item$center_fun
          }
          if ("spread_fun" %in% names(item)) {
            var_spread_funs[[item$var]] <<- item$spread_fun
          }
        } else {
          # This is a subheader - recurse into it
          is_subheader <- TRUE
          .process_level(item, current_subheader = item_name)
        }
      } else if (is.character(item) && length(item) == 1) {
        # Simple variable: name = "label"
        var_names <<- c(var_names, item_name)
        var_labels <<- c(var_labels, item)
        var_subheaders <<- c(var_subheaders, if (is.null(current_subheader)) "" else current_subheader)
      } else {
        # Unknown structure - skip with warning
        warning("Unknown structure in varlist at item: ", item_name)
      }
    }
  }
  
  # Start processing
  .process_level(varlist)
  
  # Create result data frame
  result_df <- data.frame(
    var = var_names,
    label = var_labels,
    subheader = var_subheaders,
    stringsAsFactors = FALSE
  )
  
  # Return structure
  list(
    vars = result_df,
    center_funs = var_center_funs,
    spread_funs = var_spread_funs,
    title = table_title
  )
}
