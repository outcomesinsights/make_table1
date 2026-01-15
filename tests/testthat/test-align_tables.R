test_that(".align_tables aligns tables with same structure", {
  table1 <- data.frame(
    varname = c("Age", "Sex"),
    level = c("", ""),
    statistic = c("50.0 (10.0)", "50% (50)"),
    n = c(100L, 100L),
    stringsAsFactors = FALSE
  )
  
  table2 <- data.frame(
    varname = c("Age", "Sex"),
    level = c("", ""),
    statistic = c("55.0 (12.0)", "60% (60)"),
    n = c(100L, 100L),
    stringsAsFactors = FALSE
  )
  
  aligned <- .align_tables(list(table1, table2))
  
  expect_equal(length(aligned), 2)
  expect_equal(nrow(aligned[[1]]), nrow(aligned[[2]]))
  expect_equal(aligned[[1]]$varname, aligned[[2]]$varname)
})

test_that(".align_tables handles missing rows", {
  table1 <- data.frame(
    varname = c("Age", "Sex", "Race"),
    level = c("", "", ""),
    statistic = c("50.0", "50%", "33%"),
    n = c(100L, 100L, 100L),
    stringsAsFactors = FALSE
  )
  
  table2 <- data.frame(
    varname = c("Age", "Sex"),
    level = c("", ""),
    statistic = c("55.0", "60%"),
    n = c(100L, 100L),
    stringsAsFactors = FALSE
  )
  
  aligned <- .align_tables(list(table1, table2))
  
  # table2 should be padded to match table1
  expect_equal(nrow(aligned[[1]]), nrow(aligned[[2]]))
  expect_true(any(is.na(aligned[[2]]$statistic)))  # Missing Race should be NA
})

test_that(".align_tables handles empty tables", {
  table1 <- data.frame(
    varname = c("Age"),
    level = c(""),
    statistic = c("50.0"),
    n = c(100L),
    stringsAsFactors = FALSE
  )
  
  table2 <- data.frame(
    varname = character(0),
    level = character(0),
    statistic = character(0),
    n = integer(0),
    stringsAsFactors = FALSE
  )
  
  aligned <- .align_tables(list(table1, table2))
  
  # Empty table should be filled with NA
  expect_equal(nrow(aligned[[2]]), nrow(aligned[[1]]))
})

test_that(".align_tables preserves order", {
  table1 <- data.frame(
    varname = c("A", "B", "C"),
    level = c("", "", ""),
    statistic = c("1", "2", "3"),
    n = c(10L, 10L, 10L),
    stringsAsFactors = FALSE
  )
  
  table2 <- data.frame(
    varname = c("B", "A", "C"),  # Different order
    level = c("", "", ""),
    statistic = c("2", "1", "3"),
    n = c(10L, 10L, 10L),
    stringsAsFactors = FALSE
  )
  
  aligned <- .align_tables(list(table1, table2))
  
  # Should match reference order (table1)
  expect_equal(aligned[[1]]$varname, aligned[[2]]$varname)
})
