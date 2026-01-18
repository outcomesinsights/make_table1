test_that("package functions are available", {
  test_data <- data.frame(age = c(30, 40), sex = factor(c("M", "F")))
  result <- specify_table1(test_data, vars = c("age", "sex"))
  expect_s3_class(result, "data.frame")
  expect_true(all(c("varname", "statistic") %in% names(result)))
})
