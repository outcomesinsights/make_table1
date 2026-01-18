# Create test data
test_data <- data.frame(
  age = rnorm(100, 50, 10),
  sex = factor(rep(c("M", "F"), 50)),
  treated = rep(c(TRUE, FALSE), 50),
  score = runif(100, 0, 100),
  race = factor(rep(c("White", "Black", "Asian"), length.out = 100))
)

test_that("specify_table1 works with character vector", {
  result <- specify_table1(test_data, vars = c("age", "sex", "treated"))
  
  expect_s3_class(result, "data.frame")
  expect_true("varname" %in% names(result))
  expect_true("statistic" %in% names(result))
  expect_true("n" %in% names(result))
  expect_true(nrow(result) >= 3)  # At least one row per variable
})

test_that("specify_table1 works with data frame input", {
  varlist <- data.frame(
    var = c("age", "sex", "treated"),
    label = c("Age (years)", "Sex", "Treated")
  )
  
  result <- specify_table1(test_data, vars = varlist)
  
  expect_s3_class(result, "data.frame")
  expect_true(any(result$varname == "Age (years)"))
})

test_that("specify_table1 works with nested list", {
  varlist <- list(
    "Demographics" = list(
      age = "Age (years)",
      sex = "Sex"
    ),
    "Other" = list(
      score = "Score"
    )
  )
  
  result <- specify_table1(test_data, vars = varlist)
  
  expect_s3_class(result, "data.frame")
  expect_true(any(grepl("Demographics", result$varname)))  # Subheader
  expect_true(any(result$varname == "Age (years)"))
})

test_that("specify_table1 handles subheaders", {
  varlist <- data.frame(
    var = c("**Demographics**", "age", "sex"),
    label = c("Demographics", "Age", "Sex")
  )
  
  result <- specify_table1(test_data, vars = varlist)
  
  # Should have subheader row
  expect_true(any(grepl("Demographics", result$varname)))
  demo_idx <- which(grepl("Demographics", result$varname))
  expect_true(is.na(result$statistic[demo_idx[1]]))
})

test_that("specify_table1 handles categorical variables", {
  result <- specify_table1(test_data, vars = c("race"))
  
  # Categorical should have multiple rows (one per level)
  level_rows <- grepl("^\u00A0", result$varname)
  expect_true(sum(level_rows) >= 3)  # At least 3 levels
})

test_that("specify_table1 works with custom center_fun and spread_fun", {
  result <- specify_table1(
    test_data,
    vars = c("age", "score"),
    center_fun = median,
    spread_fun = IQR
  )
  
  expect_s3_class(result, "data.frame")
  # Should have statistics (not NA)
  expect_false(all(is.na(result$statistic)))
})

test_that("specify_table1 works with group parameter columns", {
  test_data$group <- rep(c("A", "B"), 50)
  
  result <- specify_table1(
    test_data,
    vars = c("age", "sex"),
    group = "group"
  )
  
  stat_cols <- setdiff(names(result), c("varname", "n"))
  expect_true(length(stat_cols) >= 2)
  expect_true(any(names(result) == "group: A"))
  expect_true(any(names(result) == "group: B"))
})

test_that("specify_table1 handles variable-level function overrides", {
  varlist <- list(
    "Variables" = list(
      age = "Age (years)",
      score = list(
        var = "score",
        label = "Score",
        center_fun = median,
        spread_fun = IQR
      )
    )
  )
  
  result <- specify_table1(test_data, vars = varlist)
  
  expect_s3_class(result, "data.frame")
  # Both variables should be present
  expect_true(any(result$varname == "Age (years)"))
  expect_true(any(result$varname == "Score"))
})

test_that("specify_table1 handles var_types override", {
  # Force age to be treated as continuous (it already is, but test the override)
  result <- specify_table1(
    test_data,
    vars = c("age"),
    var_types = c(age = "continuous")
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= 1)
})

test_that("specify_table1 handles empty data gracefully", {
  empty_data <- data.frame(age = numeric(0), sex = character(0))
  
  expect_error(
    specify_table1(empty_data, vars = c("age")),
    NA  # Should not error, but handle gracefully
  )
})

test_that("specify_table1 validates input", {
  expect_error(specify_table1("not a data frame", vars = c("age")))
})

test_that("specify_table1 treats unknown vars as subheaders", {
  varlist <- data.frame(var = c("nonexistent"), label = c("Missing"))
  result <- specify_table1(test_data, vars = varlist)
  expect_true(any(result$varname == "Missing"))
  missing_idx <- which(result$varname == "Missing")[1]
  expect_true(is.na(result$statistic[missing_idx]))
})
