#' Align Table Structures
#'
#' Internal function to ensure all subgroup tables have the same structure
#' (same variables, same order, same number of rows) so they can be combined.
#'
#' @param tables List of data frames, each representing a subgroup table
#' @param reference_table Index of table to use as reference structure (default = 1)
#'
#' @return List of aligned data frames with consistent structure
#'
#' @keywords internal
.align_tables <- function(tables, reference_table = 1) {
  if (length(tables) == 0) {
    return(list())
  }
  
  # Use first table as reference structure
  if (reference_table > length(tables) || reference_table < 1) {
    reference_table <- 1
  }
  
  ref_table <- tables[[reference_table]]
  
  # If reference is empty, try to find a non-empty table
  if (is.null(ref_table) || nrow(ref_table) == 0) {
    for (i in seq_along(tables)) {
      if (!is.null(tables[[i]]) && nrow(tables[[i]]) > 0) {
        ref_table <- tables[[i]]
        reference_table <- i
        break
      }
    }
  }
  
  # If still no reference, return empty list
  if (is.null(ref_table) || nrow(ref_table) == 0) {
    return(tables)
  }
  
  # Get reference structure: varname and level combinations
  ref_structure <- ref_table[, c("varname", "level"), drop = FALSE]
  
  # Align each table to reference structure
  aligned_tables <- vector("list", length(tables))
  
  for (i in seq_along(tables)) {
    table_i <- tables[[i]]
    
    # If table is NULL or empty, create empty structure
    if (is.null(table_i) || nrow(table_i) == 0) {
      aligned_tables[[i]] <- data.frame(
        varname = ref_structure$varname,
        level = ref_structure$level,
        statistic = NA_character_,
        n = 0L,
        stringsAsFactors = FALSE
      )
      next
    }
    
    # Create keys for matching
    ref_key <- paste(ref_structure$varname, ref_structure$level, sep = "|||")
    table_i_key <- paste(table_i$varname, table_i$level, sep = "|||")
    
    # Match table_i rows to reference structure
    match_idx <- match(ref_key, table_i_key)
    
    # Create aligned table
    aligned <- ref_structure
    aligned$statistic <- ifelse(
      is.na(match_idx),
      NA_character_,
      table_i$statistic[match_idx]
    )
    aligned$n <- ifelse(
      is.na(match_idx),
      0L,
      table_i$n[match_idx]
    )
    
    # Handle any additional columns from table_i (like group1_stat, group2_stat, smd)
    other_cols <- setdiff(names(table_i), c("varname", "level", "statistic", "n"))
    for (col in other_cols) {
      aligned[[col]] <- ifelse(
        is.na(match_idx),
        NA,
        table_i[[col]][match_idx]
      )
    }
    
    aligned_tables[[i]] <- aligned
  }
  
  return(aligned_tables)
}
