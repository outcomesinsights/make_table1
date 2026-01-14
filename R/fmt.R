#' Format Numeric Value as Character
#'
#' Rounds numeric values and formats them with thousand separators.
#' Non-numeric values are returned unchanged. Handles edge cases like
#' empty vectors, all-NA vectors, and preserves attributes when possible.
#'
#' @param x Vector to format (numeric or other)
#' @param digits Number of digits to round to (default = 2)
#' @param na_string Character string to use for NA values (default = "NA")
#'
#' @return Character vector with formatted numbers, or original vector if non-numeric
#'
#' @examples
#' fmt(1234.567, digits = 2)
#' fmt(c(1000, 2000, 3000), digits = 0)
#' fmt(c(1.234, NA, 5.678), digits = 2)
#' fmt("text")  # returns unchanged
#' fmt(numeric(0))  # returns empty character vector
#'
#' @export
fmt <- function(x, digits = 2, na_string = "NA") {
  # Handle empty vectors
  if (length(x) == 0) {
    return(character(0))
  }
  
  # Handle non-numeric input
  if (!is.numeric(x)) {
    return(x)
  }
  
  # Round and format
  x_rounded <- round(x, digits)
  
  # Format with thousand separators
  result <- format(
    x_rounded,
    trim = TRUE,
    width = 0,
    na.encode = FALSE,
    big.mark = ",",
    format = "f",
    scientific = FALSE,
    nsmall = digits
  )
  
  # Replace NA strings if needed
  if (!is.null(na_string) && any(is.na(x))) {
    result[is.na(x)] <- na_string
  }
  
  return(result)
}
