#' @import styler

#' @export
styler::cache_activate

#' @export
styler::cache_clear

#' @export
styler::cache_deactivate

#' @export
styler::cache_info

#' @export
styler::create_style_guide

#' @export
styler::default_style_guide_attributes

#' @export
styler::specify_math_token_spacing

#' @export
styler::specify_reindention

#' @export
styler::specify_transformers_drop

#' Like [styler::style_dir()], but `style` defaulting to `equals_style`
#'
#' See [styler::style_dir()] for details, examples and more.
#' @inheritParams styler::style_dir
#' @export
style_dir <- function(path = ".",
                      ...,
                      style = equals_style,
                      transformers = style(...),
                      filetype = c("R", "Rprofile"),
                      recursive = TRUE,
                      exclude_files = NULL,
                      exclude_dirs = c("packrat", "renv"),
                      include_roxygen_examples = TRUE,
                      base_indention = 0,
                      dry = "off") {
  styler::style_dir(
    path = path,
    ...,
    transformers = transformers,
    filetype = filetype,
    recursive = recursive,
    exclude_files = exclude_files,
    exclude_dirs = exclude_dirs,
    include_roxygen_examples = include_roxygen_examples,
    base_indention = base_indention,
    dry = dry
  )
}

#' Like [styler::style_file()], but `style` defaulting to `equals_style`
#'
#' See [styler::style_file()] for details, examples and more.
#' @inheritParams styler::style_file
#' @export
style_file <- function(path,
                       ...,
                       style = equals_style,
                       transformers = style(...),
                       include_roxygen_examples = TRUE,
                       base_indention = 0,
                       dry = "off") {
  styler::style_file(
    path, ...,
    style = style, transformers = transformers,
    include_roxygen_examples = include_roxygen_examples,
    base_indention = base_indention,
    dry = dry
  )
}

#' Like [styler::style_pkg()], but `style` defaulting to `equals_style`
#'
#' See [styler::style_pkg()] for details, examples and more.
#' @inheritParams styler::style_pkg
#' @export
style_pkg <- function(pkg = ".",
                      ...,
                      style = equals_style,
                      transformers = style(...),
                      filetype = c("R", "Rprofile"),
                      exclude_files = "R/RcppExports.R",
                      exclude_dirs = c("packrat", "renv"),
                      include_roxygen_examples = TRUE,
                      base_indention = 0,
                      dry = "off") {
  styler::style_pkg(
    pkg = pkg,
    ...,
    style = style,
    transformers = transformers,
    filetype = filetype,
    exclude_files = exclude_files,
    exclude_dirs = exclude_dirs,
    include_roxygen_examples = include_roxygen_examples,
    base_indention = base_indention,
    dry = dry
  )
}


#' Like [styler::style_text()], but `style` defaulting to `equals_style`
#'
#' See [styler::style_text()] for details, examples and more.
#' @inheritParams styler::style_text
#' @export
style_text <- function(text,
                       ...,
                       style = equals_style,
                       transformers = style(...),
                       include_roxygen_examples = TRUE,
                       base_indention = 0) {
  styler::style_text(text, ...,
    style = style, transformers = transformers,
    include_roxygen_examples = include_roxygen_examples,
    base_indention = base_indention
  )
}


#' @export
styler::tidyverse_math_token_spacing

#' @export
styler::tidyverse_reindention

#' @export
styler::tidyverse_style
