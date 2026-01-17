#' table1: Create Table 1 for Descriptive Statistics
#'
#' A package for creating Table 1, which typically contains descriptive
#' information about persons under study in statistical analyses.
#' Table 1 provides summary statistics for baseline characteristics.
#'
#' The package has minimal dependencies (uses only base R for core functionality) and provides:
#' \itemize{
#'   \item Automatic variable type detection
#'   \item Flexible summary statistics (mean/median, SD/IQR, etc.)
#'   \item Single-column and multi-column table support
#'   \item Word export via flextable and officer (optional dependencies)
#'   \item Document builder pattern for creating multi-table Word reports
#'   \item Template support for Word documents
#'   \item Clean, intuitive API
#' }
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{specify_table1}}}{Create Table 1 (supports both single and multi-column tables)}
#'   \item{\code{\link{table1_to_flextable}}}{Convert Table 1 to flextable for Word export}
#'   \item{\code{\link{table1_word_doc}}}{Create or load Word document for Table 1 reports}
#'   \item{\code{\link{implement_table1}}}{Add Table 1 to Word document with optional caption}
#'   \item{\code{\link{add_section_heading}}}{Add section heading to Word document}
#'   \item{\code{\link{add_page_break}}}{Add page break to Word document}
#'   \item{\code{\link{set_orientation}}}{Set page orientation (portrait/landscape)}
#'   \item{\code{\link{save_table1_doc}}}{Save Word document to file}
#'   \item{\code{\link{fmt}}}{Format numeric values with rounding and thousand separators}
#' }
#'
#' @docType package
#' @name table1-package
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
