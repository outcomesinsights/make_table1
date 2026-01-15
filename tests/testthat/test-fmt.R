test_that("fmt formats numbers correctly", {
  expect_equal(fmt(1234.567, digits = 2), "1,234.57")
  expect_equal(fmt(1234.567, digits = 0), "1,235")
  expect_equal(fmt(1000, digits = 0), "1,000")
  expect_equal(fmt(0.123, digits = 3), "0.123")
})

test_that("fmt handles vectors", {
  result <- fmt(c(1000, 2000, 3000), digits = 0)
  expect_equal(result, c("1,000", "2,000", "3,000"))
})

test_that("fmt handles NA values", {
  result <- fmt(c(1.23, NA, 4.56), digits = 2)
  expect_true(is.na(result[2]))
  expect_equal(result[1], "1.23")
  expect_equal(result[3], "4.56")
})

test_that("fmt handles empty vectors", {
  expect_equal(fmt(numeric(0), digits = 2), character(0))
})

test_that("fmt returns non-numeric unchanged", {
  expect_equal(fmt("text"), "text")
  expect_equal(fmt(c("a", "b")), c("a", "b"))
})

test_that("fmt handles custom na_string", {
  result <- fmt(c(1.23, NA), digits = 2, na_string = "Missing")
  expect_equal(result[2], "Missing")
})
