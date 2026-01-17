# Tests for Word Document Builder Functions

test_that("table1_word_doc creates blank document when template is NULL", {
  skip_if_not_installed("officer")
  
  doc <- table1_word_doc()
  
  expect_s3_class(doc, "rdocx")
})

test_that("table1_word_doc loads template when provided", {
  skip_if_not_installed("officer")
  
  # Create a temporary template file
  temp_template <- tempfile(fileext = ".docx")
  temp_doc <- officer::read_docx()
  print(temp_doc, target = temp_template)
  
  # Test loading template
  doc <- table1_word_doc(template = temp_template)
  expect_s3_class(doc, "rdocx")
  
  # Cleanup
  unlink(temp_template)
})

test_that("table1_word_doc errors when template file not found", {
  skip_if_not_installed("officer")
  
  expect_error(
    table1_word_doc(template = "nonexistent_template.docx"),
    "Template file not found"
  )
})

test_that("add_section_heading adds heading to document", {
  skip_if_not_installed("officer")
  
  doc <- table1_word_doc() |>
    add_section_heading("Test Section")
  
  expect_s3_class(doc, "rdocx")
})

test_that("add_section_heading accepts different styles", {
  skip_if_not_installed("officer")
  
  doc <- table1_word_doc() |>
    add_section_heading("Heading 1", style = "heading 1") |>
    add_section_heading("Heading 2", style = "heading 2")
  
  expect_s3_class(doc, "rdocx")
})

test_that("implement_table1 adds table to document", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  # Create test data
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = c("M", "F", "M", "F")
  )
  
  table1 <- specify_table1(test_data, vars = c("age", "sex"))
  
  doc <- table1_word_doc() |>
    implement_table1(table1)
  
  expect_s3_class(doc, "rdocx")
})

test_that("implement_table1 adds caption when provided", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = c("M", "F", "M", "F")
  )
  
  table1 <- specify_table1(test_data, vars = c("age", "sex"))
  
  doc <- table1_word_doc() |>
    implement_table1(table1, caption = "Table 1: Test")
  
  expect_s3_class(doc, "rdocx")
})

test_that("implement_table1 passes formatting options to table1_to_flextable", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = c("M", "F", "M", "F")
  )
  
  table1 <- specify_table1(test_data, vars = c("age", "sex"))
  
  doc <- table1_word_doc() |>
    implement_table1(
      table1,
      caption = "Table 1",
      multiline_header = TRUE,
      footer_text = "Test footer"
    )
  
  expect_s3_class(doc, "rdocx")
})

test_that("add_page_break adds page break to document", {
  skip_if_not_installed("officer")
  
  doc <- table1_word_doc() |>
    add_section_heading("Section 1") |>
    add_page_break() |>
    add_section_heading("Section 2")
  
  expect_s3_class(doc, "rdocx")
})

test_that("set_orientation changes page orientation", {
  skip_if_not_installed("officer")
  
  doc <- table1_word_doc() |>
    set_orientation("portrait") |>
    set_orientation("landscape")
  
  expect_s3_class(doc, "rdocx")
})

test_that("set_orientation validates orientation argument", {
  skip_if_not_installed("officer")
  
  doc <- table1_word_doc()
  
  # Should work with valid orientation
  expect_s3_class(set_orientation(doc, "portrait"), "rdocx")
  expect_s3_class(set_orientation(doc, "landscape"), "rdocx")
})

test_that("save_table1_doc saves document to file", {
  skip_if_not_installed("officer")
  
  doc <- table1_word_doc() |>
    add_section_heading("Test Document")
  
  temp_file <- tempfile(fileext = ".docx")
  
  # Should not error
  expect_invisible(save_table1_doc(doc, temp_file))
  
  # File should exist
  expect_true(file.exists(temp_file))
  
  # Cleanup
  unlink(temp_file)
})

test_that("save_table1_doc validates file argument", {
  skip_if_not_installed("officer")
  
  doc <- table1_word_doc()
  
  expect_error(save_table1_doc(doc, NULL), "must be a single character string")
  expect_error(save_table1_doc(doc, c("file1.docx", "file2.docx")), "must be a single character string")
})

test_that("builder functions validate document object", {
  skip_if_not_installed("officer")
  
  test_data <- data.frame(age = c(30, 40), sex = c("M", "F"))
  table1 <- specify_table1(test_data, vars = c("age", "sex"))
  
  expect_error(add_section_heading("not a doc", "Test"), "must be an officer document")
  expect_error(implement_table1("not a doc", table1), "must be an officer document")
  expect_error(add_page_break("not a doc"), "must be an officer document")
  expect_error(set_orientation("not a doc", "portrait"), "must be an officer document")
  expect_error(save_table1_doc("not a doc", "file.docx"), "must be an officer document")
})

test_that("integration test: build complete document", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = c("M", "F", "M", "F"),
    group = c("A", "A", "B", "B")
  )
  
  table1_single <- specify_table1(test_data, vars = c("age", "sex"))
  table1_multi <- specify_table1(test_data, vars = c("age", "sex"), group = "group")
  
  temp_file <- tempfile(fileext = ".docx")
  
  doc <- table1_word_doc() |>
    add_section_heading("Section 1: Single Column Table") |>
    implement_table1(table1_single, caption = "Table 1: Single Column") |>
    add_page_break() |>
    set_orientation("portrait") |>
    add_section_heading("Section 2: Multi Column Table") |>
    implement_table1(
      table1_multi,
      caption = "Table 2: Multi Column",
      multiline_header = TRUE,
      footer_text = "Note: Test footer"
    ) |>
    set_orientation("landscape")
  
  save_table1_doc(doc, temp_file)
  
  expect_true(file.exists(temp_file))
  
  # Cleanup
  unlink(temp_file)
})
