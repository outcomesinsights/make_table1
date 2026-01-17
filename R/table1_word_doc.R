#' Create or Load Word Document for Table 1
#'
#' Creates a new Word document or loads an existing template for building
#' Table 1 documents. This is the starting point for the document builder pattern.
#'
#' @param template Character string, path to a Word template file (.docx).
#'   If NULL (default), creates a blank document.
#'
#' @return An \code{officer} document object.
#'
#' @details
#' This function initializes a Word document for building Table 1 reports.
#' You can start with a blank document or load an existing template that
#' contains styles, headers, footers, or other formatting.
#'
#' @examples
#' \dontrun{
#' # Create blank document
#' doc <- table1_word_doc()
#'
#' # Load template
#' doc <- table1_word_doc(template = "./templates/report_template.docx")
#' }
#'
#' @export
table1_word_doc <- function(template = NULL) {
  # Check for officer package
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("The 'officer' package is required. Install with: install.packages('officer')")
  }
  
  if (is.null(template)) {
    # Create blank document
    doc <- officer::read_docx()
  } else {
    # Load template
    if (!file.exists(template)) {
      stop("Template file not found: ", template)
    }
    doc <- officer::read_docx(template)
  }
  
  return(doc)
}

#' Add Section Heading to Word Document
#'
#' Adds a section heading paragraph to a Word document.
#'
#' @param doc An \code{officer} document object.
#' @param text Character string, the heading text.
#' @param style Character string, the paragraph style. Default: "heading 1".
#' @param pos Character string, position to add the heading. Options: "after" (default)
#'   or "before" (adds at the beginning).
#'
#' @return The modified \code{officer} document object.
#'
#' @examples
#' \dontrun{
#' doc <- table1_word_doc() |>
#'   add_section_heading("Patient Characteristics")
#' }
#'
#' @export
add_section_heading <- function(doc, text, style = "heading 1", pos = "after") {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("The 'officer' package is required. Install with: install.packages('officer')")
  }
  
  if (!inherits(doc, "rdocx")) {
    stop("'doc' must be an officer document object from table1_word_doc()")
  }
  
  doc <- officer::body_add_par(doc, value = text, style = style, pos = pos)
  return(doc)
}

#' Add Table 1 to Word Document
#'
#' Converts a Table 1 result to a flextable and adds it to a Word document,
#' optionally with a caption. If \code{data}, \code{vars}, and \code{subgroups}
#' are provided, automatically creates individual tables for each subgroup
#' instead of using \code{table1_result}.
#'
#' @param doc An \code{officer} document object.
#' @param table1_result Output from \code{\link{specify_table1}}. Ignored if
#'   \code{data}, \code{vars}, and \code{subgroups} are provided.
#' @param caption Character string, optional table caption. For individual
#'   tables, this is used as a prefix with subgroup name appended.
#' @param caption_style Character string, style for the caption. Default: "caption".
#' @param data Data frame containing the data. If provided along with \code{vars}
#'   and \code{subgroups}, individual tables are created for each subgroup.
#' @param vars Variable specification (as used in \code{\link{specify_table1}}).
#' @param subgroups Named list of filter functions for creating individual tables.
#' @param section_prefix Character string prefix for section headings when
#'   creating individual tables. Default: "Tables for".
#' @param caption_prefix Character string prefix for table captions when
#'   creating individual tables. Default: "Table 1:  Population Characteristics for".
#' @param caption_suffix Character string suffix for table captions when
#'   creating individual tables. Default: " Patients".
#' @param section_style Character string, paragraph style for section headings.
#'   Default: "heading 1".
#' @param add_page_breaks Logical, whether to add page breaks between individual
#'   tables. Default: TRUE.
#' @param first_table_pos Character string, position for first table section heading.
#'   Options: "before" or "after" (default).
#' @param digits Numeric, number of decimal places for summary statistics when
#'   creating individual tables. Default: 1.
#' @param ... Additional arguments passed to \code{\link{table1_to_flextable}}.
#'
#' @return The modified \code{officer} document object.
#'
#' @details
#' If \code{data}, \code{vars}, and \code{subgroups} are all provided, this
#' function automatically creates individual Table 1 tables for each subgroup
#' by filtering the data and adding them to the document with section headings,
#' captions, and page breaks. Otherwise, it adds the \code{table1_result} as-is.
#'
#' @examples
#' \dontrun{
#' # Single table
#' table1 <- specify_table1(data, vars = c("age", "sex"))
#' doc <- table1_word_doc() |>
#'   implement_table1(table1, caption = "Table 1: Patient Characteristics")
#'
#' # Individual tables for each subgroup (automatic)
#' doc <- table1_word_doc() |>
#'   implement_table1(
#'     data = dt_prep,
#'     vars = varlist,
#'     subgroups = list(
#'       "All Diagnosed" = function(d) rep(TRUE, nrow(d)),
#'       "No Treatment" = function(d) d$treatment == "None"
#'     ),
#'     footer_text = "Note: Results presented as % (count)..."
#'   )
#' }
#'
#' @export
implement_table1 <- function(doc,
                      table1_result = NULL,
                      caption = NULL,
                      caption_style = "caption",
                      data = NULL,
                      vars = NULL,
                      subgroups = NULL,
                      section_prefix = "Tables for",
                      caption_prefix = "Table 1:  Population Characteristics for",
                      caption_suffix = " Patients",
                      section_style = "heading 1",
                      add_page_breaks = TRUE,
                      first_table_pos = "after",
                      digits = 1,
                      ...) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("The 'officer' package is required. Install with: install.packages('officer')")
  }
  
  if (!inherits(doc, "rdocx")) {
    stop("'doc' must be an officer document object from table1_word_doc()")
  }
  
  # Check if we should create individual tables
  create_individual <- !is.null(data) && !is.null(vars) && !is.null(subgroups)
  
  if (create_individual) {
    # Create individual tables for each subgroup
    if (!is.data.frame(data)) {
      stop("'data' must be a data frame")
    }
    
    if (is.null(subgroups) || length(subgroups) == 0) {
      stop("'subgroups' must be a non-empty named list of filter functions")
    }
    
    subgroup_names <- names(subgroups)
    if (is.null(subgroup_names) || any(subgroup_names == "")) {
      stop("'subgroups' must have non-empty names")
    }
    
    # Loop through each subgroup
    for (i in seq_along(subgroup_names)) {
      group_name <- subgroup_names[i]
      filter_func <- subgroups[[group_name]]
      
      # Validate filter function
      if (!is.function(filter_func)) {
        stop("Subgroup '", group_name, "' must be a function")
      }
      
      # Filter data
      filter_mask <- tryCatch(
        filter_func(data),
        error = function(e) {
          stop("Error applying filter for subgroup '", group_name, "': ", e$message)
        }
      )
      
      if (!is.logical(filter_mask) || length(filter_mask) != nrow(data)) {
        stop("Filter function for subgroup '", group_name, "' must return a logical vector of length nrow(data)")
      }
      
      dti <- data[filter_mask, , drop = FALSE]
      
      # Create table for this subgroup
      table1_single <- specify_table1(
        data = dti,
        vars = vars,
        digits = digits
      )
      
      # Create section heading and caption
      sectcap <- paste0(section_prefix, " ", group_name, caption_suffix)
      tab1cap <- if (is.null(caption)) {
        paste0(
          caption_prefix,
          group_name,
          caption_suffix,
          " (N = ",
          fmt(nrow(dti), 0),
          ")"
        )
      } else {
        paste0(caption, ": ", group_name, " (N = ", fmt(nrow(dti), 0), ")")
      }
      
      # Determine position for first table
      table_pos <- if (i == 1) first_table_pos else "after"
      
      # Add to document
      doc <- doc |>
        add_section_heading(sectcap, style = section_style, pos = table_pos) |>
        implement_table1(table1_single, caption = tab1cap, caption_style = caption_style, ...)
      
      # Add page break if requested (but not after the last table)
      if (add_page_breaks && i < length(subgroup_names)) {
        doc <- add_page_break(doc)
      }
    }
  } else {
    # Use table1_result as-is
    if (is.null(table1_result)) {
      stop("Either 'table1_result' must be provided, or 'data', 'vars', and 'subgroups' must all be provided")
    }
    
    # Add caption if provided
    if (!is.null(caption)) {
      caption_obj <- officer::block_caption(label = caption, style = caption_style)
      doc <- officer::body_add_caption(doc, caption_obj)
    }
    
    # Convert to flextable and add to document
    ft <- table1_to_flextable(table1_result, ...)
    if (!requireNamespace("flextable", quietly = TRUE)) {
      stop("The 'flextable' package is required. Install with: install.packages('flextable')")
    }
    doc <- flextable::body_add_flextable(doc, ft)
  }
  
  return(doc)
}

#' Add Page Break to Word Document
#'
#' Adds a page break to a Word document.
#'
#' @param doc An \code{officer} document object.
#'
#' @return The modified \code{officer} document object.
#'
#' @examples
#' \dontrun{
#' doc <- table1_word_doc() |>
#'   implement_table1(table1_1, caption = "Table 1") |>
#'   add_page_break() |>
#'   implement_table1(table1_2, caption = "Table 2")
#' }
#'
#' @export
add_page_break <- function(doc) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("The 'officer' package is required. Install with: install.packages('officer')")
  }
  
  if (!inherits(doc, "rdocx")) {
    stop("'doc' must be an officer document object from table1_word_doc()")
  }
  
  doc <- officer::body_add_break(doc)
  return(doc)
}

#' Set Page Orientation for Word Document
#'
#' Changes the page orientation (portrait or landscape) for subsequent content
#' in a Word document. The orientation applies to all content added after this call.
#'
#' @param doc An \code{officer} document object.
#' @param orientation Character string, page orientation. Options: "portrait" (default)
#'   or "landscape".
#' @param type Character string, section break type. Default: "continuous".
#'   Other options: "nextPage", "evenPage", "oddPage".
#'
#' @return The modified \code{officer} document object.
#'
#' @details
#' In Word documents, orientation is defined at the section level and applies
#' to all content that comes before the next section break. This function creates
#' a section break with the specified orientation.
#'
#' @examples
#' \dontrun{
#' doc <- table1_word_doc() |>
#'   implement_table1(table1_portrait, caption = "Table 1 (Portrait)") |>
#'   set_orientation("landscape") |>
#'   implement_table1(table1_landscape, caption = "Table 2 (Landscape)")
#' }
#'
#' @export
set_orientation <- function(doc, orientation = c("portrait", "landscape"), type = "continuous") {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("The 'officer' package is required. Install with: install.packages('officer')")
  }
  
  if (!inherits(doc, "rdocx")) {
    stop("'doc' must be an officer document object from table1_word_doc()")
  }
  
  orientation <- match.arg(orientation)
  
  # Create section block with specified orientation
  section_block <- officer::block_section(
    officer::prop_section(
      page_size = officer::page_size(orient = orientation),
      type = type
    )
  )
  
  # Apply section break
  doc <- officer::body_end_block_section(doc, value = section_block)
  
  return(doc)
}

#' Save Word Document
#'
#' Saves a Word document to a file.
#'
#' @param doc An \code{officer} document object.
#' @param file Character string, path to the output file (.docx).
#'
#' @return Invisibly returns the document object.
#'
#' @examples
#' \dontrun{
#' doc <- table1_word_doc() |>
#'   implement_table1(table1, caption = "Table 1")
#' save_table1_doc(doc, "output/table1.docx")
#' }
#'
#' @export
save_table1_doc <- function(doc, file) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("The 'officer' package is required. Install with: install.packages('officer')")
  }
  
  if (!inherits(doc, "rdocx")) {
    stop("'doc' must be an officer document object from table1_word_doc()")
  }
  
  if (is.null(file) || !is.character(file) || length(file) != 1) {
    stop("'file' must be a single character string specifying the output file path")
  }
  
  # Save document
  print(doc, target = file)
  
  return(invisible(doc))
}
