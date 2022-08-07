# see the files in tests/testthat/core: There is an input and an expected output version.
# tests fail if the output of the style transformation is not equal to the expected output
# also see `help('styler:::test_collection', 'styler')`
test_that("line break for multi-line function declaration", {
  expect_warning(styler:::test_collection("core", "eq-sub-replacement",
    transformer = style_text,
  ), NA)
})
