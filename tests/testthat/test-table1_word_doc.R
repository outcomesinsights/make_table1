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
    sex = factor(c("M", "F", "M", "F"))
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
    sex = factor(c("M", "F", "M", "F"))
  )
  
  table1 <- specify_table1(test_data, vars = c("age", "sex"))
  
  doc <- table1_word_doc() |>
    implement_table1(table1, caption = "Table 1: Test", caption_style = "Normal")
  
  expect_s3_class(doc, "rdocx")
})

test_that("implement_table1 passes formatting options to table1_to_flextable", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = factor(c("M", "F", "M", "F"))
  )
  
  table1 <- specify_table1(test_data, vars = c("age", "sex"))
  
  doc <- table1_word_doc() |>
    implement_table1(
      table1,
      caption = "Table 1",
      caption_style = "Normal",
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
  
  test_data <- data.frame(age = c(30, 40), sex = factor(c("M", "F")))
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
    sex = factor(c("M", "F", "M", "F")),
    group = c("A", "A", "B", "B")
  )
  
  table1_single <- specify_table1(test_data, vars = c("age", "sex"))
  table1_multi <- specify_table1(test_data, vars = c("age", "sex"), group = "group")
  
  temp_file <- tempfile(fileext = ".docx")
  
  doc <- table1_word_doc() |>
    add_section_heading("Section 1: Single Column Table") |>
    implement_table1(table1_single, caption = "Table 1: Single Column", caption_style = "Normal") |>
    add_page_break() |>
    set_orientation("portrait") |>
    add_section_heading("Section 2: Multi Column Table") |>
    implement_table1(
      table1_multi,
      caption = "Table 2: Multi Column",
      caption_style = "Normal",
      multiline_header = TRUE,
      footer_text = "Note: Test footer"
    ) |>
    set_orientation("landscape")
  
  save_table1_doc(doc, temp_file)
  
  expect_true(file.exists(temp_file))
  
  # Cleanup
  unlink(temp_file)
})

# Tests for new features: Automatic multi-table creation

test_that("implement_table1 creates individual tables with subgroups", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60, 35, 45),
    sex = factor(c("M", "F", "M", "F", "M", "F")),
    group = c("A", "A", "B", "B", "A", "B")
  )
  
  subgroups <- list(
    "Group A" = function(d) d$group == "A",
    "Group B" = function(d) d$group == "B"
  )
  
  doc <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      include_multi_table = FALSE,  # Don't create multi-table for this test
      caption_style = "Normal"
    )
  
  expect_s3_class(doc, "rdocx")
})

test_that("implement_table1 automatically creates multi-table when include_multi_table is TRUE", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60, 35, 45),
    sex = factor(c("M", "F", "M", "F", "M", "F")),
    group = c("A", "A", "B", "B", "A", "B")
  )
  
  subgroups <- list(
    "Group A" = function(d) d$group == "A",
    "Group B" = function(d) d$group == "B"
  )
  
  doc <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      include_multi_table = TRUE,  # Should create multi-table automatically
      caption_style = "Normal"
    )
  
  expect_s3_class(doc, "rdocx")
})

test_that("implement_table1 includes_multi_table defaults to TRUE", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = factor(c("M", "F", "M", "F")),
    group = c("A", "A", "B", "B")
  )
  
  subgroups <- list(
    "Group A" = function(d) d$group == "A",
    "Group B" = function(d) d$group == "B"
  )
  
  # Should create multi-table by default (include_multi_table not specified)
  doc <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      caption_style = "Normal"
    )
  
  expect_s3_class(doc, "rdocx")
})

# Tests for orientation controls

test_that("implement_table1 respects subtable_orientation parameter", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = factor(c("M", "F", "M", "F")),
    group = c("A", "A", "B", "B")
  )
  
  subgroups <- list(
    "Group A" = function(d) d$group == "A",
    "Group B" = function(d) d$group == "B"
  )
  
  # Test portrait orientation
  doc_portrait <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      subtable_orientation = "portrait",
      include_multi_table = FALSE,
      caption_style = "Normal"
    )
  
  expect_s3_class(doc_portrait, "rdocx")
  
  # Test landscape orientation
  doc_landscape <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      subtable_orientation = "landscape",
      include_multi_table = FALSE,
      caption_style = "Normal"
    )
  
  expect_s3_class(doc_landscape, "rdocx")
})

test_that("implement_table1 respects multi_table_orientation parameter", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = factor(c("M", "F", "M", "F")),
    group = c("A", "A", "B", "B")
  )
  
  subgroups <- list(
    "Group A" = function(d) d$group == "A",
    "Group B" = function(d) d$group == "B"
  )
  
  # Test landscape orientation (default)
  doc_landscape <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      include_multi_table = TRUE,
      multi_table_orientation = "landscape",
      caption_style = "Normal"
    )
  
  expect_s3_class(doc_landscape, "rdocx")
  
  # Test portrait orientation
  doc_portrait <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      include_multi_table = TRUE,
      multi_table_orientation = "portrait",
      caption_style = "Normal"
    )
  
  expect_s3_class(doc_portrait, "rdocx")
})

test_that("implement_table1 validates orientation parameters", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = factor(c("M", "F", "M", "F")),
    group = c("A", "A", "B", "B")
  )
  
  subgroups <- list(
    "Group A" = function(d) d$group == "A",
    "Group B" = function(d) d$group == "B"
  )
  
  # Should work with valid orientations
  doc1 <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      subtable_orientation = "portrait",
      multi_table_orientation = "landscape",
      include_multi_table = FALSE,
      caption_style = "Normal"
    )
  
  expect_s3_class(doc1, "rdocx")
  
  # Should error with invalid orientation
  expect_error(
    table1_word_doc() |>
      implement_table1(
        data = test_data,
        vars = c("age", "sex"),
        subgroups = subgroups,
        subtable_orientation = "invalid",
        include_multi_table = FALSE
      ),
    "should be one of"
  )
})

# Tests for multi-table header text

test_that("implement_table1 uses multi_table_section_heading when provided", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = factor(c("M", "F", "M", "F")),
    group = c("A", "A", "B", "B")
  )
  
  subgroups <- list(
    "Group A" = function(d) d$group == "A",
    "Group B" = function(d) d$group == "B"
  )
  
  doc <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      include_multi_table = TRUE,
      multi_table_section_heading = "Tables for All Patients",
      caption_style = "Normal"
    )
  
  expect_s3_class(doc, "rdocx")
})

test_that("implement_table1 uses multi_table_caption when provided", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = factor(c("M", "F", "M", "F")),
    group = c("A", "A", "B", "B")
  )
  
  subgroups <- list(
    "Group A" = function(d) d$group == "A",
    "Group B" = function(d) d$group == "B"
  )
  
  doc <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      include_multi_table = TRUE,
      multi_table_caption = "Table 1: All Patients Combined",
      caption_style = "Normal"
    )
  
  expect_s3_class(doc, "rdocx")
})

test_that("implement_table1 constructs multi_table_caption from prefix/suffix when not provided", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = factor(c("M", "F", "M", "F")),
    group = c("A", "A", "B", "B")
  )
  
  subgroups <- list(
    "Group A" = function(d) d$group == "A",
    "Group B" = function(d) d$group == "B"
  )
  
  doc <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      include_multi_table = TRUE,
      caption_prefix = "Table 1: Characteristics for",
      caption_suffix = " Patients",
      caption_style = "Normal"
      # multi_table_caption not provided, should use prefix + "All Patients" + suffix
    )
  
  expect_s3_class(doc, "rdocx")
})

test_that("implement_table1 respects multi_table_section_style", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = factor(c("M", "F", "M", "F")),
    group = c("A", "A", "B", "B")
  )
  
  subgroups <- list(
    "Group A" = function(d) d$group == "A",
    "Group B" = function(d) d$group == "B"
  )
  
  doc <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      include_multi_table = TRUE,
      multi_table_section_heading = "All Patients",
      multi_table_section_style = "heading 2",
      caption_style = "Normal"
    )
  
  expect_s3_class(doc, "rdocx")
})

# Tests for multi-table parameters

test_that("implement_table1 passes multi_table_include_all to specify_table1", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = factor(c("M", "F", "M", "F")),
    group = c("A", "A", "B", "B")
  )
  
  subgroups <- list(
    "Group A" = function(d) d$group == "A",
    "Group B" = function(d) d$group == "B"
  )
  
  doc <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      include_multi_table = TRUE,
      multi_table_include_all = TRUE,
      multi_table_all_label = "All Groups",
      caption_style = "Normal"
    )
  
  expect_s3_class(doc, "rdocx")
})

test_that("implement_table1 passes multi_table_empty_subgroup_handling to specify_table1", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60),
    sex = factor(c("M", "F", "M", "F")),
    group = c("A", "A", "B", "B")
  )
  
  subgroups <- list(
    "Group A" = function(d) d$group == "A",
    "Group Z" = function(d) d$group == "Z"  # Empty subgroup
  )
  
  doc <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      include_multi_table = TRUE,
      multi_table_empty_subgroup_handling = "zero",
      caption_style = "Normal"
    )
  
  expect_s3_class(doc, "rdocx")
})

# Tests for warning on excessive groups

test_that("implement_table1 warns when subgroups > 6", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = rep(30:40, 7),
    sex = factor(rep(c("M", "F"), length.out = 77)),
    group = rep(1:7, each = 11)
  )
  
  # Create 7 subgroups (should trigger warning)
  subgroups <- list(
    "Group 1" = function(d) d$group == 1,
    "Group 2" = function(d) d$group == 2,
    "Group 3" = function(d) d$group == 3,
    "Group 4" = function(d) d$group == 4,
    "Group 5" = function(d) d$group == 5,
    "Group 6" = function(d) d$group == 6,
    "Group 7" = function(d) d$group == 7
  )
  
  expect_warning(
    table1_word_doc() |>
      implement_table1(
        data = test_data,
        vars = c("age", "sex"),
        subgroups = subgroups,
        include_multi_table = TRUE,
        caption_style = "Normal"
      ),
    "more than 6 groups"
  )
})

test_that("implement_table1 does not warn when subgroups <= 6", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = rep(30:40, 6),
    sex = factor(rep(c("M", "F"), length.out = 66)),
    group = rep(1:6, each = 11)
  )
  
  # Create 6 subgroups (should NOT trigger warning)
  subgroups <- list(
    "Group 1" = function(d) d$group == 1,
    "Group 2" = function(d) d$group == 2,
    "Group 3" = function(d) d$group == 3,
    "Group 4" = function(d) d$group == 4,
    "Group 5" = function(d) d$group == 5,
    "Group 6" = function(d) d$group == 6
  )
  
  # Should not produce warning
  result <- expect_silent(
    table1_word_doc() |>
      implement_table1(
        data = test_data,
        vars = c("age", "sex"),
        subgroups = subgroups,
        include_multi_table = TRUE,
        caption_style = "Normal"
      )
  )
  
  expect_s3_class(result, "rdocx")
})

test_that("implement_table1 does not warn when include_multi_table is FALSE", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = rep(30:40, 7),
    sex = factor(rep(c("M", "F"), length.out = 77)),
    group = rep(1:7, each = 11)
  )
  
  # Create 7 subgroups but don't create multi-table
  subgroups <- list(
    "Group 1" = function(d) d$group == 1,
    "Group 2" = function(d) d$group == 2,
    "Group 3" = function(d) d$group == 3,
    "Group 4" = function(d) d$group == 4,
    "Group 5" = function(d) d$group == 5,
    "Group 6" = function(d) d$group == 6,
    "Group 7" = function(d) d$group == 7
  )
  
  # Should not produce warning since include_multi_table is FALSE
  result <- expect_silent(
    table1_word_doc() |>
      implement_table1(
        data = test_data,
        vars = c("age", "sex"),
        subgroups = subgroups,
        include_multi_table = FALSE,  # No multi-table, so no warning
        caption_style = "Normal"
      )
  )
  
  expect_s3_class(result, "rdocx")
})

# Integration test with all new features

test_that("integration test: all new features work together", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  
  test_data <- data.frame(
    age = c(30, 40, 50, 60, 35, 45),
    sex = factor(c("M", "F", "M", "F", "M", "F")),
    group = c("A", "A", "B", "B", "A", "B")
  )
  
  subgroups <- list(
    "Group A" = function(d) d$group == "A",
    "Group B" = function(d) d$group == "B"
  )
  
  temp_file <- tempfile(fileext = ".docx")
  
  doc <- table1_word_doc() |>
    implement_table1(
      data = test_data,
      vars = c("age", "sex"),
      subgroups = subgroups,
      include_multi_table = TRUE,
      subtable_orientation = "portrait",
      multi_table_orientation = "landscape",
      multi_table_section_heading = "Combined Analysis",
      multi_table_caption = "Table 1: All Groups Combined",
      multi_table_section_style = "heading 1",
      multi_table_include_all = FALSE,
      multi_table_all_label = "All",
      multi_table_empty_subgroup_handling = "na",
      caption_style = "Normal",
      footer_text = "Note: Test footer"
    )
  
  save_table1_doc(doc, temp_file)
  
  expect_true(file.exists(temp_file))
  
  # Cleanup
  unlink(temp_file)
})
