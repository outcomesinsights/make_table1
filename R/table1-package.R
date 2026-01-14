#' table1: Create Table 1 for Descriptive Statistics
#'
#' A package for creating Table 1, which typically contains descriptive
#' information about persons under study in statistical analyses.
#' Table 1 provides summary statistics for baseline characteristics.
#'
#' The package has zero dependencies (uses only base R) and provides:
#' \itemize{
#'   \item Automatic variable type detection
#'   \item Flexible summary statistics (mean/median, SD/IQR, etc.)
#'   \item Optional standardized mean differences (SMD) for group comparisons
#'   \item Clean, intuitive API
#' }
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{make_table1}}}{Create Table 1 with optional SMD calculation}
#'   \item{\code{\link{make_table1_multi}}}{Create multi-column Table 1 for multiple subgroups}
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
