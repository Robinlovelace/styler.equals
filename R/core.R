version = unlist(unname(read.dcf("DESCRIPTION")[, "Version"]))

#' The equals style
#'
#' Style code according to the equals style guide. For more
#' details and docs, see the [styler::tidyverse_style()].
#' @inheritParams styler::tidyverse_style
#' @family obtain transformers
#' @family style_guides
#' @examples
#' style_text("call( 1)", scope = "spaces")
#' style_text("x <- 1")
#' @importFrom purrr partial
#' @export
equals_style = function(scope = "tokens",
                                        strict = TRUE,
                                        indent_by = 2,
                                        start_comments_with_one_space = FALSE,
                                        reindention = tidyverse_reindention(),
                                        math_token_spacing = tidyverse_math_token_spacing()) {
  # derive from tidyverse
  transformers = styler::tidyverse_style(
    scope=scope,
    strict = strict,
    indent_by = indent_by,
    start_comments_with_one_space = start_comments_with_one_space,
    reindention = reindention,
    math_token_spacing = math_token_spacing
  )
  transformers$style_guide_name = "styler.equals::equals_stylehttps://github.com/Robinlovelace"
  transformers$style_guide_version = version

  # reverse tranformer to make <- into =
  if ('tokens' %in% scope_normalize(scope)) {
    transformers$token$force_assignment_op = force_assignment_eq
    # also overwrite rules for transformer dropping
    # help(specify_transformers_drop)
    transformers$transformers_drop$token$force_assignment_op = "LEFT_ASSIGN"

  }

  transformers
}
