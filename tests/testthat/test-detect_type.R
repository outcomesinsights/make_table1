test_that(".detect_type identifies continuous variables", {
  data <- data.frame(
    age = rnorm(100, 50, 10),
    score = 1:100
  )
  
  expect_equal(.detect_type(data, "age"), "continuous")
  expect_equal(.detect_type(data, "score"), "continuous")
})

test_that(".detect_type identifies binary variables", {
  data <- data.frame(
    treated = c(TRUE, FALSE, TRUE, FALSE),
    sex = factor(c("M", "F", "M", "F")),
    binary = c(0, 1, 0, 1)
  )
  
  expect_equal(.detect_type(data, "treated"), "binary")
  expect_equal(.detect_type(data, "sex"), "binary")
  expect_equal(.detect_type(data, "binary"), "binary")
})

test_that(".detect_type identifies categorical variables", {
  data <- data.frame(
    race = factor(c("White", "Black", "Asian", "White", "Black"))
  )
  
  expect_equal(.detect_type(data, "race"), "categorical")
})

test_that(".detect_type handles empty variables", {
  data <- data.frame(
    empty = rep(NA, 10)
  )
  
  expect_equal(.detect_type(data, "empty"), "empty")
})

test_that(".detect_type handles Date variables", {
  data <- data.frame(
    date = as.Date(c("2020-01-01", "2020-01-02", "2020-01-03"))
  )
  
  expect_equal(.detect_type(data, "date"), "continuous")
})

test_that(".detect_type handles single value variables", {
  data <- data.frame(
    constant = rep(5, 10)
  )
  
  expect_equal(.detect_type(data, "constant"), "binary")
})
