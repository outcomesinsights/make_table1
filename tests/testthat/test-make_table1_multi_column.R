# Create test data
test_data <- data.frame(
  age = rnorm(200, 50, 10),
  sex = factor(rep(c("M", "F"), 100)),
  treated = rep(c(TRUE, FALSE), 100),
  group = rep(c("A", "B", "C", "D"), 50)
)

test_that("make_table1 works with grouping variable", {
  result <- make_table1(
    test_data,
    vars = c("age", "sex", "treated"),
    group = "group",
    include_all = TRUE
  )
  
  expect_s3_class(result, "data.frame")
  # Should have columns for All and each group level
  expect_true("All" %in% names(result))
  expect_true(ncol(result) > 4)  # varname, All, and group columns
})

test_that("make_table1 works with subgroups parameter", {
  result <- make_table1(
    test_data,
    vars = c("age", "sex", "treated"),
    subgroups = list(Group = "group"),
    include_all = TRUE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("All" %in% names(result))
  expect_true(ncol(result) > 4)
})

test_that("make_table1 works with custom filters", {
  result <- make_table1(
    test_data,
    vars = c("age", "sex"),
    subgroups = list(
      "Treated" = function(d) d$treated == TRUE,
      "Untreated" = function(d) d$treated == FALSE
    ),
    include_all = TRUE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true("All" %in% names(result))
  expect_true("Treated" %in% names(result))
  expect_true("Untreated" %in% names(result))
})

test_that("make_table1 handles empty subgroups", {
  result <- make_table1(
    test_data,
    vars = c("age", "sex"),
    subgroups = list(
      "Group A" = function(d) d$group == "A",
      "Group Z" = function(d) d$group == "Z"  # Doesn't exist
    ),
    empty_subgroup_handling = "na"
  )
  
  expect_s3_class(result, "data.frame")
  # Should still have structure even with empty subgroup
})

test_that("make_table1 handles empty_subgroup_handling options", {
  # Test "zero" option
  result_zero <- make_table1(
    test_data,
    vars = c("age"),
    subgroups = list(
      "Group Z" = function(d) d$group == "Z"
    ),
    empty_subgroup_handling = "zero"
  )
  
  expect_s3_class(result_zero, "data.frame")
  
  # Test "skip" option
  result_skip <- make_table1(
    test_data,
    vars = c("age"),
    subgroups = list(
      "Group A" = function(d) d$group == "A",
      "Group Z" = function(d) d$group == "Z"
    ),
    empty_subgroup_handling = "skip"
  )
  
  expect_s3_class(result_skip, "data.frame")
  expect_false("Group Z" %in% names(result_skip))
})

test_that("make_table1 works without include_all", {
  result <- make_table1(
    test_data,
    vars = c("age", "sex"),
    subgroups = list(
      "Treated" = function(d) d$treated == TRUE
    ),
    include_all = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_false("All" %in% names(result))
  expect_true("Treated" %in% names(result))
})

test_that("make_table1 aligns tables correctly", {
  result <- make_table1(
    test_data,
    vars = c("age", "sex", "treated"),
    subgroups = list(
      "Group A" = function(d) d$group == "A",
      "Group B" = function(d) d$group == "B"
    ),
    include_all = TRUE
  )
  
  # All columns should have same number of rows
  n_rows <- nrow(result)
  for (col in names(result)) {
    if (col != "varname") {
      expect_equal(length(result[[col]]), n_rows)
    }
  }
})

test_that("make_table1 validates input for multi-column tables", {
  expect_error(make_table1("not a data frame", vars = c("age")))
  expect_error(
    make_table1(
      test_data,
      vars = c("age"),
      subgroups = NULL,
      include_all = FALSE
    )
  )
})
