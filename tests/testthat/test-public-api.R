test_that("general", {
  withr::local_options(styler.quiet = TRUE, usethis.quiet = TRUE)
  expect_equal(
    style_text("x = 1 ", scope = "tokens") %>%
      as.character(),
    "x = 1"
  )
  example_code = "x <- 1    # comment"
  file = withr::local_tempfile(fileext = ".R")
  writeLines(example_code, file)
  style_file(file, strict = FALSE)

  expect_equal(readLines(file), "x = 1    # comment")

  dir = withr::local_tempdir()
  file = fs::path(dir, ".Rprofile")
  writeLines(example_code, file)
  style_dir(dir, include_roxygen_examples = FALSE)
  expect_equal(readLines(file), "x = 1    # comment")

  dir = withr::local_tempdir(pattern = "pkg")
  usethis::create_package(dir, open = FALSE)
  file = fs::path(dir, "R", "script.R")
  writeLines(" a %>% c", file)
  style_pkg(dir)
  expect_equal(readLines(file), "a %>% c")
})
