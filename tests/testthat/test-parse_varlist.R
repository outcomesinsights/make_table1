test_that(".parse_varlist handles simple nested list", {
  varlist <- list(
    "Demographics" = list(
      age = "Age (years)",
      sex = "Sex"
    )
  )
  
  result <- .parse_varlist(varlist)
  
  expect_true("vars" %in% names(result))
  expect_true("center_funs" %in% names(result))
  expect_true("spread_funs" %in% names(result))
  
  expect_equal(result$vars$var, c("age", "sex"))
  expect_equal(result$vars$label, c("Age (years)", "Sex"))
  expect_equal(result$title, "Demographics")
  expect_equal(result$vars$subheader, c("", ""))
})

test_that(".parse_varlist handles table title", {
  varlist <- list(
    "Table Title" = list(
      "Demographics" = list(
        age = "Age"
      )
    )
  )
  
  result <- .parse_varlist(varlist)
  
  expect_equal(result$title, "Table Title")
  expect_equal(result$vars$subheader, "Demographics")
})

test_that(".parse_varlist handles structured variables with overrides", {
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
  
  result <- .parse_varlist(varlist)
  
  expect_equal(result$vars$var, c("age", "score"))
  expect_true("score" %in% names(result$center_funs))
  expect_true("score" %in% names(result$spread_funs))
  expect_equal(result$center_funs$score, median)
  expect_equal(result$spread_funs$score, IQR)
})

test_that(".parse_varlist handles multiple subheaders", {
  varlist <- list(
    "Demographics" = list(
      age = "Age",
      sex = "Sex"
    ),
    "Treatment" = list(
      treated = "Treated"
    )
  )
  
  result <- .parse_varlist(varlist)
  
  expect_equal(result$vars$subheader, c("Demographics", "Demographics", "Treatment"))
})

test_that(".parse_varlist captures per-variable options", {
  varlist <- list(
    "Variables" = list(
      status = list(
        var = "status",
        label = "Status",
        levels = c("Yes", "No"),
        combine_remaining = TRUE,
        other_label = "Other",
        digits = 1,
        binary_display = list(levels = "single", value = "yes", layout = "one_row")
      )
    )
  )
  
  result <- .parse_varlist(varlist)
  
  expect_equal(result$digits$status, 1)
  expect_true(isTRUE(result$combine_remaining$status))
  expect_equal(result$other_label$status, "Other")
  expect_true(is.list(result$binary_display$status))
})

test_that(".parse_varlist handles deep nesting", {
  varlist <- list(
    "Table Title" = list(
      "Section 1" = list(
        "Subsection A" = list(
          var1 = "Variable 1"
        )
      )
    )
  )
  
  result <- .parse_varlist(varlist)
  
  # Should use the most specific subheader
  expect_true(nrow(result$vars) >= 1)
})
