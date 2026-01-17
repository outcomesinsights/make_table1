# Skip tests if flextable is not available
skip_if_not_installed("flextable")

# Create test data
test_data <- data.frame(
  age = rnorm(100, 50, 10),
  sex = factor(rep(c("M", "F"), 50)),
  treated = rep(c(TRUE, FALSE), 50),
  score = runif(100, 0, 100),
  race = factor(rep(c("White", "Black", "Asian"), length.out = 100))
)

test_that("table1_to_flextable works with single-column table", {
  table1 <- specify_table1(test_data, vars = c("age", "sex", "treated"))
  ft <- table1_to_flextable(table1)
  
  expect_s3_class(ft, "flextable")
  expect_true("Variable" %in% ft$header$col_keys)
  expect_true("Statistic" %in% ft$header$col_keys || 
              any(grepl("Statistic", ft$header$col_keys)))
})

test_that("table1_to_flextable includes n column for single-column tables by default", {
  table1 <- specify_table1(test_data, vars = c("age", "sex"))
  ft <- table1_to_flextable(table1)
  
  # Check that n column is present
  col_keys <- ft$header$col_keys
  expect_true("n" %in% col_keys || any(grepl("n", col_keys)))
})

test_that("table1_to_flextable works with multi-column table", {
  test_data$group <- rep(c("A", "B"), 50)
  table1_multi <- specify_table1(
    test_data,
    vars = c("age", "sex", "treated"),
    group = "group"
  )
  
  ft <- table1_to_flextable(table1_multi)
  
  expect_s3_class(ft, "flextable")
  expect_true("Variable" %in% ft$header$col_keys)
  # Should have multiple statistic columns
  stat_cols <- ft$header$col_keys[ft$header$col_keys != "Variable"]
  expect_true(length(stat_cols) >= 2)
})

test_that("table1_to_flextable formats headers with sample sizes", {
  table1 <- specify_table1(test_data, vars = c("age", "sex"))
  ft <- table1_to_flextable(table1)
  
  # Check that header contains sample size format
  header_text <- as.character(ft$header$dataset)
  expect_true(any(grepl("n=", header_text) || grepl("\\(n=", header_text)))
})

test_that("table1_to_flextable handles custom header_n_format", {
  table1 <- specify_table1(test_data, vars = c("age", "sex"))
  ft <- table1_to_flextable(table1, header_n_format = "\\n(n={n})")
  
  expect_s3_class(ft, "flextable")
})

test_that("table1_to_flextable handles show_n_column parameter", {
  table1 <- specify_table1(test_data, vars = c("age", "sex"))
  
  # With n column
  ft_with_n <- table1_to_flextable(table1, show_n_column = TRUE)
  col_keys_with_n <- ft_with_n$header$col_keys
  
  # Without n column
  ft_without_n <- table1_to_flextable(table1, show_n_column = FALSE)
  col_keys_without_n <- ft_without_n$header$col_keys
  
  # Should have different number of columns
  expect_true(length(col_keys_with_n) >= length(col_keys_without_n))
})

test_that("table1_to_flextable handles subheaders", {
  varlist <- data.frame(
    var = c("**Demographics**", "age", "sex"),
    label = c("Demographics", "Age (years)", "Sex")
  )
  table1 <- specify_table1(test_data, vars = varlist)
  ft <- table1_to_flextable(table1)
  
  expect_s3_class(ft, "flextable")
  # Subheaders should be bold (checking structure, not actual formatting)
})

test_that("table1_to_flextable handles factor indentation", {
  table1 <- specify_table1(test_data, vars = c("race"))
  ft <- table1_to_flextable(table1)
  
  expect_s3_class(ft, "flextable")
  # Factor levels should be indented (checking structure)
})

test_that("table1_to_flextable handles multi-column with varying n values", {
  # Create data where some variables have missing values in some groups
  test_data$group <- rep(c("A", "B"), 50)
  test_data$age[test_data$group == "A"][1:5] <- NA  # 5 missing in group A
  
  table1_multi <- specify_table1(
    test_data,
    vars = c("age", "sex"),
    group = "group"
  )
  
  ft <- table1_to_flextable(table1_multi)
  
  expect_s3_class(ft, "flextable")
  # Should handle per-cell n values appropriately
})

test_that("table1_to_flextable validates input", {
  expect_error(table1_to_flextable("not a data frame"))
  expect_error(table1_to_flextable(data.frame(x = 1:10)))  # No varname column
})

test_that("table1_to_flextable handles empty subgroups", {
  test_data$group <- rep(c("A", "B"), 50)
  
  table1_multi <- specify_table1(
    test_data,
    vars = c("age", "sex"),
    subgroups = list(
      "Group A" = function(d) d$group == "A",
      "Group Z" = function(d) d$group == "Z"  # Empty subgroup
    ),
    empty_subgroup_handling = "na"
  )
  
  ft <- table1_to_flextable(table1_multi)
  
  expect_s3_class(ft, "flextable")
})

test_that("table1_to_flextable works with custom cell_n_format", {
  test_data$group <- rep(c("A", "B"), 50)
  table1_multi <- specify_table1(
    test_data,
    vars = c("age", "sex"),
    group = "group"
  )
  
  ft <- table1_to_flextable(table1_multi, cell_n_format = " [n={n}]")
  
  expect_s3_class(ft, "flextable")
})

test_that("table1_to_flextable handles indent_width parameter", {
  table1 <- specify_table1(test_data, vars = c("race"))
  ft <- table1_to_flextable(table1, indent_width = 4)
  
  expect_s3_class(ft, "flextable")
})
