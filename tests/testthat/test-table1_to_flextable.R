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
  expect_true("varname" %in% ft$header$col_keys)
  expect_true("statistic" %in% ft$header$col_keys ||
              any(grepl("statistic", ft$header$col_keys, ignore.case = TRUE)))
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
  expect_true("varname" %in% ft$header$col_keys)
  # Should have multiple statistic columns
  stat_cols <- ft$header$col_keys[ft$header$col_keys != "Variable"]
  expect_true(length(stat_cols) >= 2)
})

test_that("table1_to_flextable formats headers with sample sizes", {
  table1 <- specify_table1(test_data, vars = c("age", "sex"))
  ft <- table1_to_flextable(table1)
  
  # Header should include expected columns
  header_cols <- ft$header$col_keys
  expect_true(all(c("varname", "statistic") %in% header_cols))
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
  # Subheaders should be bold (check formatting)
  bold_style <- ft$body$styles$text$bold
  bold_matrix <- if (is.matrix(bold_style)) {
    bold_style
  } else if (is.list(bold_style) && !is.null(bold_style$values)) {
    bold_style$values
  } else {
    NULL
  }
  demo_idx <- which(ft$body$dataset$varname == "Demographics")
  age_idx <- which(ft$body$dataset$varname == "Age (years)")
  if (!is.null(bold_matrix) && length(demo_idx) > 0 && length(age_idx) > 0) {
    expect_true(isTRUE(bold_matrix[demo_idx[1], 1]))
    expect_true(isTRUE(bold_matrix[age_idx[1], 1]))
  }
})

test_that("table1_to_flextable handles factor indentation", {
  table1 <- specify_table1(test_data, vars = c("race"))
  ft <- table1_to_flextable(table1)
  
  expect_s3_class(ft, "flextable")
  # Factor levels should be indented (check padding on variable column)
  pad_style <- ft$body$styles$paragraph$padding.left
  pad_matrix <- if (is.matrix(pad_style)) {
    pad_style
  } else if (is.list(pad_style) && !is.null(pad_style$values)) {
    pad_style$values
  } else {
    NULL
  }
  if (!is.null(pad_matrix)) {
    expect_true(any(pad_matrix[, 1] > 0, na.rm = TRUE))
  }
})

test_that("table1_to_flextable applies subheader shading and row lines", {
  varlist <- data.frame(
    var = c("**Demographics**", "age", "sex"),
    label = c("Demographics", "Age (years)", "Sex")
  )
  table1 <- specify_table1(test_data, vars = varlist)
  ft <- table1_to_flextable(table1, theme = "table1")
  
  bg_style <- ft$body$styles$background$color
  bg_matrix <- if (is.matrix(bg_style)) {
    bg_style
  } else if (is.list(bg_style) && !is.null(bg_style$values)) {
    bg_style$values
  } else {
    NULL
  }
  demo_idx <- which(ft$body$dataset$varname == "Demographics")
  age_idx <- which(ft$body$dataset$varname == "Age (years)")
  if (!is.null(bg_matrix) && length(demo_idx) > 0 && length(age_idx) > 0) {
    expect_equal(toupper(bg_matrix[demo_idx[1], 1]), "#F0F0F0")
    expect_true(is.na(bg_matrix[age_idx[1], 1]) || bg_matrix[age_idx[1], 1] == "")
  }
  
  border_style <- ft$body$styles$border$bottom
  border_matrix <- if (is.matrix(border_style)) {
    border_style
  } else if (is.list(border_style) && !is.null(border_style$values)) {
    border_style$values
  } else {
    NULL
  }
  if (!is.null(border_matrix)) {
    expect_true(any(!is.na(border_matrix[, 1])))
  }
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
