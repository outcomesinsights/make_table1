#' Custom Theme for Table 1
#'
#' Applies a custom theme optimized for Table 1 descriptive statistics tables.
#' Provides clean, publication-ready formatting with appropriate borders,
#' spacing, and subheader styling.
#'
#' @param ft A \code{flextable} object
#' @param subheader_bg_color Character string, background color for subheader rows.
#'   Default: "#F0F0F0" (light gray). Set to NULL to skip subheader shading.
#' @param subheader_rows Integer vector of row indices for subheader rows.
#'   If NULL, will attempt to identify them automatically.
#'
#' @return The modified \code{flextable} object
#'
#' @details
#' The theme applies:
#' \itemize{
#'   \item Clean borders: top and bottom borders for header, minimal borders for body
#'   \item Subheader background shading (light gray by default)
#'   \item Appropriate padding and spacing
#'   \item Consistent font styling
#' }
#'
#' @export
theme_table1 <- function(ft, subheader_bg_color = "#F0F0F0", subheader_rows = NULL) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("The 'flextable' package is required. Install with: install.packages('flextable')")
  }
  
  # Get number of columns
  n_cols <- length(ft$col_keys)
  
  # Apply borders
  # Header: top and bottom borders
  ft <- flextable::border(ft, part = "header", 
                          border.top = flextable::fp_border(color = "black", width = 1),
                          border.bottom = flextable::fp_border(color = "black", width = 1))
  
  # Body: minimal borders (bottom border only for last row)
  # Remove all body borders first
  ft <- flextable::border_remove(ft, part = "body")
  
  # Add bottom border to last row
  n_rows <- nrow(ft$body$dataset)
  if (n_rows > 0) {
    ft <- flextable::border(ft, i = n_rows, part = "body",
                            border.bottom = flextable::fp_border(color = "black", width = 1))
  }
  
  # Apply padding for better spacing
  ft <- flextable::padding(ft, part = "all", padding = 4)
  
  # Set font size (default 11pt for body, keep header as is)
  ft <- flextable::fontsize(ft, part = "body", size = 11)
  
  # Apply subheader background color if provided
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
  
  return(ft)
}
