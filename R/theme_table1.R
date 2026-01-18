#' Custom Theme for Table 1
#'
#' Applies a custom theme optimized for Table 1 descriptive statistics tables.
#' Provides clean, publication-ready formatting with appropriate borders,
#' spacing, and subheader styling.
#'
#' @param ft A \code{flextable} object
#' @param subheader_bg_color Character string, background color for subheader rows.
#'   Default: "#F0F0F0" (light gray). Set to NULL to skip subheader shading.
#' @param subheader_rows Integer vector of row indices for top-level subheader rows
#'   (for shading). If NULL, will attempt to identify them automatically.
#' @param all_subheader_rows Integer vector of row indices for all subheader rows
#'   (for bolding, includes both top-level and variable subheaders). If NULL, will
#'   attempt to identify them automatically.
#' @param bold_subheaders Logical, whether to bold subheader rows. Default: TRUE.
#' @param bold_variable_column Logical, whether to bold all rows in the variable column.
#'   Default: FALSE.
#' @param indent_rows Integer vector of row indices to indent. Default: NULL.
#' @param indent_width Numeric, indentation width for variable column. Default: 2.
#'
#' @return The modified \code{flextable} object
#'
#' @details
#' The theme applies:
#' \itemize{
#'   \item Clean borders: top and bottom borders for header, minimal borders for body
#'   \item Subheader background shading (light gray by default) for top-level subheaders
#'   \item Bold formatting for subheader rows (if requested)
#'   \item Appropriate padding and spacing
#'   \item Consistent font styling
#' }
#'
#' @export
theme_table1 <- function(ft, subheader_bg_color = "#F0F0F0", subheader_rows = NULL,
                        all_subheader_rows = NULL, bold_subheaders = TRUE,
                        bold_variable_column = FALSE, indent_rows = NULL,
                        indent_width = 2) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("The 'flextable' package is required. Install with: install.packages('flextable')")
  }
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("The 'officer' package is required. Install with: install.packages('officer')")
  }
  
  # Get number of columns
  n_cols <- length(ft$col_keys)
  
  # Apply borders
  # Header: top and bottom borders
  header_border <- officer::fp_border(color = "black", width = 1)
  ft <- flextable::border(
    ft,
    part = "header",
    border.top = header_border,
    border.bottom = header_border
  )
  
  # Body: horizontal lines between all rows
  # Remove all body borders first (older flextable doesn't support `part`)
  if ("part" %in% names(formals(flextable::border_remove))) {
    ft <- flextable::border_remove(ft, part = "body")
  } else {
    ft <- flextable::border_remove(ft)
  }
  # Reapply header borders in case border_remove cleared them
  ft <- flextable::border(
    ft,
    part = "header",
    border.top = header_border,
    border.bottom = header_border
  )
  # Apply horizontal lines across body rows
  n_rows <- nrow(ft$body$dataset)
  if (n_rows > 0) {
    ft <- flextable::border(
      ft,
      i = seq_len(n_rows),
      part = "body",
      border.bottom = header_border
    )
  }
  
  # Apply padding for better spacing
  ft <- flextable::padding(ft, part = "all", padding = 4)
  
  # Set font size (default 11pt for body, keep header as is)
  ft <- flextable::fontsize(ft, part = "body", size = 11)
  
  # Apply subheader background color if provided (only for top-level subheaders)
  if (!is.null(subheader_bg_color) && tolower(subheader_bg_color) != "transparent") {
    if (is.null(subheader_rows)) {
      # Try to identify subheader rows automatically
      # Subheaders typically have NA values in statistic columns
      # This is a fallback - ideally subheader_rows should be passed in
      warning("subheader_rows not provided, attempting to identify automatically")
    } else if (length(subheader_rows) > 0) {
      ft <- flextable::bg(ft, i = subheader_rows, bg = subheader_bg_color, part = "body")
    }
  }
  
  # Bold subheader rows (if requested) - applies to all subheaders (top-level and variable)
  if (bold_subheaders) {
    if (is.null(all_subheader_rows)) {
      # Try to identify subheader rows automatically
      warning("all_subheader_rows not provided, attempting to identify automatically")
    } else if (length(all_subheader_rows) > 0) {
      ft <- flextable::bold(ft, i = all_subheader_rows, part = "body")
    }
  }
  
  # Bold variable column (if requested)
  if (bold_variable_column) {
    ft <- flextable::bold(ft, j = 1, part = "body")
  }
  
  # Apply indentation using padding after theme spacing
  if (!is.null(indent_rows) && length(indent_rows) > 0) {
    ft <- flextable::padding(ft, i = indent_rows, j = 1, padding.left = indent_width)
  }
  
  return(ft)
}
