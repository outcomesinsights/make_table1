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
#' optionally with a caption.
#'
#' @param doc An \code{officer} document object.
#' @param table1_result Output from \code{\link{make_table1}}.
#' @param caption Character string, optional table caption. If NULL (default),
#'   no caption is added.
#' @param caption_style Character string, style for the caption. Default: "caption".
#' @param ... Additional arguments passed to \code{\link{table1_to_flextable}}.
#'
#' @return The modified \code{officer} document object.
#'
#' @details
#' This function combines \code{\link{table1_to_flextable}} and
#' \code{officer::body_add_flextable()} to add a formatted Table 1 to the document.
#' All formatting options from \code{table1_to_flextable()} can be passed via \code{...}.
#'
#' @examples
#' \dontrun{
#' table1 <- make_table1(data, vars = c("age", "sex"))
#' doc <- table1_word_doc() |>
#'   add_table1(table1, caption = "Table 1: Patient Characteristics")
#'
#' # With formatting options
#' doc <- table1_word_doc() |>
#'   add_table1(
#'     table1,
#'     caption = "Table 1: Characteristics",
#'     multiline_header = TRUE,
#'     footer_text = "Note: Results presented as % (count) for categorical variables..."
#'   )
#' }
#'
#' @export
add_table1 <- function(doc, table1_result, caption = NULL, caption_style = "caption", ...) {
  if (!requireNamespace("officer", quietly = TRUE)) {
    stop("The 'officer' package is required. Install with: install.packages('officer')")
  }
  
  if (!inherits(doc, "rdocx")) {
    stop("'doc' must be an officer document object from table1_word_doc()")
  }
  
  # Add caption if provided
  if (!is.null(caption)) {
    caption_obj <- officer::block_caption(label = caption, style = caption_style)
    doc <- officer::body_add_caption(doc, caption_obj)
  }
  
  # Convert to flextable and add to document
  ft <- table1_to_flextable(table1_result, ...)
  doc <- officer::body_add_flextable(doc, ft)
  
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
#'   add_table1(table1_1, caption = "Table 1") |>
#'   add_page_break() |>
#'   add_table1(table1_2, caption = "Table 2")
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
#'   add_table1(table1_portrait, caption = "Table 1 (Portrait)") |>
#'   set_orientation("landscape") |>
#'   add_table1(table1_landscape, caption = "Table 2 (Landscape)")
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
#'   add_table1(table1, caption = "Table 1")
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
