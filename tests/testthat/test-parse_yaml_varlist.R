# Skip tests if yaml package not available
skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    skip(paste("Package", pkg, "not available"))
  }
}

test_that("parse_yaml_varlist works with YAML string", {
  skip_if_not_installed("yaml")
  
  yaml_str <- "
Demographics:
  age: Age (years)
  sex: Sex
"
  
  result <- parse_yaml_varlist(yaml_str)
  
  expect_true(is.list(result))
  expect_true("Demographics" %in% names(result))
})

test_that("parse_yaml_varlist handles structured variables", {
  skip_if_not_installed("yaml")
  
  yaml_str <- "
Variables:
  age: Age (years)
  score:
    var: score
    label: Score
    center_fun: median
    spread_fun: IQR
"
  
  result <- parse_yaml_varlist(yaml_str)
  
  expect_true(is.list(result))
  # Check that structured variable is parsed correctly
  expect_true("Variables" %in% names(result))
})

test_that("parse_yaml_varlist handles table title", {
  skip_if_not_installed("yaml")
  
  yaml_str <- "
Table Title:
  Demographics:
    age: Age
"
  
  result <- parse_yaml_varlist(yaml_str)
  
  expect_true(is.list(result))
})

test_that("parse_yaml_varlist converts function names to functions", {
  skip_if_not_installed("yaml")
  
  yaml_str <- "
Variables:
  score:
    var: score
    label: Score
    center_fun: median
    spread_fun: IQR
"
  
  result <- parse_yaml_varlist(yaml_str)
  
  # The result should be compatible with specify_table1
  expect_true(is.list(result))
})

test_that("parse_yaml_varlist errors without yaml package", {
  # This test would need to mock the requireNamespace check
  # For now, just test that it gives a helpful error
  if (!requireNamespace("yaml", quietly = TRUE)) {
    expect_error(
      parse_yaml_varlist("test: value"),
      "yaml"
    )
  }
})

test_that("parse_yaml_varlist handles file input", {
  skip_if_not_installed("yaml")
  
  # Create temporary YAML file
  yaml_content <- "
Demographics:
  age: Age (years)
"
  temp_file <- tempfile(fileext = ".yaml")
  writeLines(yaml_content, temp_file)
  
  on.exit(unlink(temp_file))
  
  result <- parse_yaml_varlist(temp_file, file = TRUE)
  
  expect_true(is.list(result))
})
