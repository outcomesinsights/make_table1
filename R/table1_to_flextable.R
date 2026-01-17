#' Convert Table 1 to flextable for Word Export
#'
#' Converts the output from \code{\link{specify_table1}} into a formatted
#' \code{flextable} object suitable for Word export. Handles sample sizes
#' in headers and per-cell n values when they differ from column headers.
#'
#' @param table1_result Output from \code{\link{specify_table1}}
#' @param show_n_column Logical, whether to show n column for single-column tables.
#'   If NULL (default), automatically set to TRUE for single-column, FALSE for multi-column.
#' @param header_n_format Character string format for sample size in headers.
#'   Use \code{{n}} as placeholder. Default: \code{"(n={n})"}.
#'   Examples: \code{"(n={n})"}, \code{"\n(n={n})"}, \code{" ({n})"}.
#' @param cell_n_format Character string format for appending n to cell values when
#'   they differ from header. Use \code{{n}} as placeholder. Default: \code{", n={n}"}.
#' @param indent_width Numeric, width of indentation for factor levels in units
#'   (default = 2). Applied using flextable padding.
#' @param multiline_header Logical, whether to create multi-line headers with format
#'   description. If TRUE, creates 3-line header: (1) Variable/Group name, (2) N = sample size,
#'   (3) Format description. Default: FALSE.
#' @param format_description Character string for format description line in multi-line headers.
#'   Default: \code{"% (N) or Mean (SD)"}.
#' @param footer_text Character string to add as footer row. If NULL (default), no footer is added.
#' @param table_width Numeric, table width as proportion of page (0-1). Default: 1.0.
#' @param table_layout Character, table layout. Options: "autofit" (default), "fixed", "auto".
#' @param theme Character, flextable theme. Options: "vanilla" (default), "booktabs", "alafoli", etc.
#'   Set to NULL to skip theme application.
#' @param format_line_fontsize Numeric, font size for format description line (line 3) in
#'   multi-line headers. Default: 9.
#' @param merge_variable_header Logical, whether to merge Variable column header across
#'   all header rows in multi-line headers. Default: TRUE when multiline_header is TRUE.
#' @param bold_header Logical, whether to bold header rows. Default: TRUE.
#' @param bold_variable_column Logical, whether to bold all rows in the variable column.
#'   Default: FALSE.
#' @param bold_subheaders Logical, whether to bold subheader rows (rows with NA statistics).
#'   Default: TRUE.
#' @param ... Additional arguments passed to \code{flextable::flextable()}
#'
#' @return A \code{flextable} object formatted for Word export.
#'
#' @details
#' For single-column tables:
#' \itemize{
#'   \item Headers include sample size: "Statistic (n=100)"
#'   \item An n column is included showing per-row sample sizes
#' }
#'
#' For multi-column tables:
#' \itemize{
#'   \item Each column header includes the column's total sample size
#'   \item When a cell's sample size differs from the column header, it's appended
#'     to the statistic (e.g., "45.2 (10.5), n=95" when header says n=100)
#'   \item Subheaders are bolded by default
#'   \item Factor levels are indented
#' }
#'
#' When \code{multiline_header = TRUE}, creates a 3-line header structure:
#' \itemize{
#'   \item Line 1: Variable/Group names
#'   \item Line 2: N = sample sizes
#'   \item Line 3: Format description (e.g., "% (N) or Mean (SD)")
#' }
#'
#' @examples
#' \dontrun{
#' # Single-column table
#' table1 <- specify_table1(data, vars = c("age", "sex"))
#' ft <- table1_to_flextable(table1)
#' 
#' # Multi-column table with multi-line headers and custom formatting
#' table1_multi <- specify_table1(data, vars = c("age", "sex"), group = "group")
#' ft <- table1_to_flextable(
#'   table1_multi,
#'   multiline_header = TRUE,
#'   bold_header = TRUE,
#'   bold_subheaders = TRUE,
#'   bold_variable_column = FALSE,
#'   footer_text = "Note: Results presented as % (count) for categorical variables..."
#' )
#' 
#' # Export to Word using document builder
#' library(table1)
#' doc <- table1_word_doc() |>
#'   implement_table1(table1_multi, caption = "Table 1: Characteristics")
#' save_table1_doc(doc, "table1.docx")
#' }
#'
#' @export
table1_to_flextable <- function(table1_result,
                                show_n_column = NULL,
                                header_n_format = "(n={n})",
                                cell_n_format = ", n={n}",
                                indent_width = 2,
                                multiline_header = FALSE,
                                format_description = "% (N) or Mean (SD)",
                                footer_text = NULL,
                                table_width = 1.0,
                                table_layout = "autofit",
                                theme = "vanilla",
                                format_line_fontsize = 9,
                                merge_variable_header = NULL,
                                bold_header = TRUE,
                                bold_variable_column = FALSE,
                                bold_subheaders = TRUE,
                                ...) {
  
  # Check for flextable package
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("The 'flextable' package is required. Install with: install.packages('flextable')")
  }
  
  # Validate input
  if (!is.data.frame(table1_result)) {
    stop("'table1_result' must be a data frame from specify_table1()")
  }
  
  if (!"varname" %in% names(table1_result)) {
    stop("'table1_result' must have a 'varname' column")
  }
  
  # Set default for merge_variable_header
  if (is.null(merge_variable_header)) {
    merge_variable_header <- multiline_header
  }
  
  # Detect table type: single-column has "statistic" column, multi-column has multiple statistic columns
  has_statistic_col <- "statistic" %in% names(table1_result)
  statistic_cols <- if (has_statistic_col) {
    "statistic"
  } else {
    # Multi-column: find all columns that are not varname, n, or n_*
    all_cols <- names(table1_result)
    all_cols[!all_cols %in% c("varname", "n") & !grepl("^n_", all_cols)]
  }
  
  is_single_column <- has_statistic_col && length(statistic_cols) == 1
  
  # Set default for show_n_column
  if (is.null(show_n_column)) {
    show_n_column <- is_single_column
  }
  
  # Extract sample sizes
  if (is_single_column) {
    # Single-column: get total n from n column (max or first non-NA)
    if ("n" %in% names(table1_result)) {
      total_n <- max(table1_result$n, na.rm = TRUE)
      if (is.infinite(total_n)) total_n <- NA_integer_
    } else {
      total_n <- NA_integer_
    }
    header_ns <- setNames(total_n, "Statistic")
  } else {
    # Multi-column: get from attribute
    header_ns <- attr(table1_result, "sample_sizes")
    if (is.null(header_ns)) {
      # Fallback: try to extract from n_* columns
      n_cols <- names(table1_result)[grepl("^n_", names(table1_result))]
      if (length(n_cols) > 0) {
        # Get max n from each n_* column
        header_ns <- sapply(n_cols, function(col) {
          max(table1_result[[col]], na.rm = TRUE)
        })
        names(header_ns) <- sub("^n_", "", names(header_ns))
        # Replace Inf with NA
        header_ns[is.infinite(header_ns)] <- NA_integer_
      } else {
        # No sample size information available
        header_ns <- setNames(rep(NA_integer_, length(statistic_cols)), statistic_cols)
      }
    }
  }
  
  # Prepare data for flextable
  ft_data <- table1_result
  
  # Process cell values for multi-column tables
  if (!is_single_column) {
    # For each statistic column, check if cell n differs from header n
    for (stat_col in statistic_cols) {
      n_col <- paste0("n_", stat_col)
      
      if (n_col %in% names(ft_data)) {
        header_n <- header_ns[[stat_col]]
        if (!is.na(header_n)) {
          # Compare each cell's n to header n
          cell_ns <- ft_data[[n_col]]
          diff_mask <- !is.na(cell_ns) & !is.na(ft_data[[stat_col]]) & 
                       cell_ns != header_n
          
          if (any(diff_mask, na.rm = TRUE)) {
            # Append n value to statistics where n differs
            for (i in which(diff_mask)) {
              cell_n <- cell_ns[i]
              # Replace {n} placeholder with actual n value
              formatted_n <- gsub("\\{n\\}", as.character(cell_n), cell_n_format)
              ft_data[[stat_col]][i] <- paste0(ft_data[[stat_col]][i], formatted_n)
            }
          }
        }
      }
    }
  }
  
  # Build column structure for flextable
  if (is_single_column) {
    # Single-column: [varname, statistic, n]
    display_cols <- c("varname", "statistic")
    if (show_n_column && "n" %in% names(ft_data)) {
      display_cols <- c(display_cols, "n")
    }
    
    # Build headers
    header_labels <- c("Variable", "Statistic")
    if (show_n_column && "n" %in% names(ft_data)) {
      header_labels <- c(header_labels, "n")
    }
    
    # Add sample size to Statistic header
    if (!is.na(header_ns[["Statistic"]])) {
      header_labels[header_labels == "Statistic"] <- paste0(
        "Statistic ",
        gsub("\\{n\\}", as.character(header_ns[["Statistic"]]), header_n_format)
      )
    }
    
  } else {
    # Multi-column: [varname, stat_col1, stat_col2, ...]
    display_cols <- c("varname", statistic_cols)
    
    # Build headers with sample sizes
    header_labels <- c("Variable", statistic_cols)
    for (i in seq_along(statistic_cols)) {
      col_name <- statistic_cols[i]
      header_n <- header_ns[[col_name]]
      if (!is.na(header_n)) {
        header_labels[i + 1] <- paste0(
          col_name, " ",
          gsub("\\{n\\}", as.character(header_n), header_n_format)
        )
      }
    }
  }
  
  # Select only display columns (exclude n_* columns)
  ft_data_display <- ft_data[, display_cols, drop = FALSE]
  
  # Identify indented rows (factor levels - rows starting with non-breaking spaces)
  # Do this before cleaning so we know which rows to indent
  indent_rows <- integer(0)
  if (nrow(ft_data_display) > 0) {
    varname_col <- ft_data_display[["varname"]]
    # Check for rows that start with non-breaking space or have leading spaces
    for (i in seq_along(varname_col)) {
      varname_val <- varname_col[i]
      if (!is.na(varname_val) && nchar(varname_val) > 0) {
        # Check if starts with non-breaking space (U+00A0) or regular spaces
        first_char <- substr(varname_val, 1, 1)
        if (first_char == "\u00A0" || first_char == " ") {
          indent_rows <- c(indent_rows, i)
        }
      }
    }
  }
  
  # Clean varname column: remove leading spaces/non-breaking spaces for cleaner display
  # (indentation will be applied via padding instead)
  if (length(indent_rows) > 0) {
    for (i in indent_rows) {
      varname_val <- ft_data_display[["varname"]][i]
      # Remove leading non-breaking spaces and regular spaces
      cleaned <- gsub("^[\u00A0 ]+", "", varname_val)
      ft_data_display[["varname"]][i] <- cleaned
    }
  }
  
  # Identify subheader rows (rows with NA statistics)
  subheader_rows <- integer(0)
  for (stat_col in statistic_cols) {
    na_rows <- which(is.na(ft_data_display[[stat_col]]))
    subheader_rows <- unique(c(subheader_rows, na_rows))
  }
  
  # Create flextable
  if (multiline_header) {
    # Multi-line header: create header data frame
    header_info <- data.frame(
      col_keys = display_cols,
      line1 = character(length(display_cols)),
      line2 = character(length(display_cols)),
      line3 = character(length(display_cols)),
      stringsAsFactors = FALSE
    )
    
    # Line 1: Variable/Group names
    header_info$line1[1] <- "Variable"
    if (is_single_column) {
      header_info$line1[2] <- "Statistic"
      if (show_n_column && "n" %in% display_cols) {
        header_info$line1[3] <- "n"
      }
    } else {
      for (i in seq_along(statistic_cols)) {
        header_info$line1[i + 1] <- statistic_cols[i]
      }
    }
    
    # Line 2: N = sample sizes
    header_info$line2[1] <- "Variable"
    if (is_single_column) {
      if (!is.na(header_ns[["Statistic"]])) {
        header_info$line2[2] <- paste0("N = ", fmt(header_ns[["Statistic"]], 0))
      } else {
        header_info$line2[2] <- "N = "
      }
      if (show_n_column && "n" %in% display_cols) {
        header_info$line2[3] <- "n"
      }
    } else {
      for (i in seq_along(statistic_cols)) {
        col_name <- statistic_cols[i]
        header_n <- header_ns[[col_name]]
        if (!is.na(header_n)) {
          header_info$line2[i + 1] <- paste0("N = ", fmt(header_n, 0))
        } else {
          header_info$line2[i + 1] <- "N = "
        }
      }
    }
    
    # Line 3: Format description
    header_info$line3[1] <- "Variable"
    if (is_single_column) {
      header_info$line3[2] <- format_description
      if (show_n_column && "n" %in% display_cols) {
        header_info$line3[3] <- "n"
      }
    } else {
      for (i in seq_along(statistic_cols)) {
        header_info$line3[i + 1] <- format_description
      }
    }
    
    # Create flextable with multi-line header
    ft <- flextable::flextable(ft_data_display, col_keys = display_cols, ...)
    ft <- flextable::set_header_df(ft, mapping = header_info, key = "col_keys")
    
    # Merge Variable column header if requested
    if (merge_variable_header) {
      ft <- flextable::merge_v(ft, part = "header", j = 1)
    }
    
    # Set font size for format description line (line 3)
    ft <- flextable::fontsize(ft, i = 3, size = format_line_fontsize, part = "header")
    
  } else {
    # Single-line header
    ft <- flextable::flextable(ft_data_display, ...)
    
    # Set header labels
    ft <- flextable::set_header_labels(ft, values = setNames(header_labels, display_cols))
  }
  
  # Format headers: bold (if requested) and center
  if (bold_header) {
    ft <- flextable::bold(ft, part = "header")
  }
  ft <- flextable::align(ft, align = "center", part = "header")
  
  # Bold subheader rows (if requested)
  if (bold_subheaders && length(subheader_rows) > 0) {
    ft <- flextable::bold(ft, i = subheader_rows)
  }
  
  # Bold variable column (if requested)
  if (bold_variable_column) {
    ft <- flextable::bold(ft, j = 1)
  }
  
  # Apply indentation using padding
  if (length(indent_rows) > 0) {
    ft <- flextable::padding(ft, i = indent_rows, j = 1, padding.left = indent_width)
  }
  
  # Align columns
  # Left-align varname column
  ft <- flextable::align(ft, j = 1, align = "left")
  
  # Center-align statistic columns
  if (length(statistic_cols) > 0) {
    stat_col_indices <- which(names(ft_data_display) %in% statistic_cols)
    for (j in stat_col_indices) {
      ft <- flextable::align(ft, j = j, align = "center")
    }
  }
  
  # Right-align n column if present
  if (show_n_column && "n" %in% names(ft_data_display)) {
    n_col_idx <- which(names(ft_data_display) == "n")
    if (length(n_col_idx) > 0) {
      ft <- flextable::align(ft, j = n_col_idx, align = "right")
    }
  }
  
  # Apply table properties
  ft <- flextable::set_table_properties(ft, layout = table_layout, width = table_width)
  
  # Apply theme
  if (!is.null(theme)) {
    theme_func <- switch(theme,
      "vanilla" = flextable::theme_vanilla,
      "booktabs" = flextable::theme_booktabs,
      "alafoli" = flextable::theme_alafoli,
      "box" = flextable::theme_box,
      "tron" = flextable::theme_tron,
      "vader" = flextable::theme_vader,
      "zebra" = flextable::theme_zebra,
      NULL
    )
    if (!is.null(theme_func)) {
      ft <- theme_func(ft)
    } else {
      warning("Unknown theme '", theme, "'. Available themes: vanilla, booktabs, alafoli, box, tron, vader, zebra")
    }
  }
  
  # Add footer if provided
  if (!is.null(footer_text)) {
    ft <- flextable::add_footer_row(
      ft,
      values = footer_text,
      colwidths = ncol(ft_data_display)
    )
  }
  
  return(ft)
}
