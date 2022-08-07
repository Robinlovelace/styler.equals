version <- unlist(unname(read.dcf("DESCRIPTION")[, "Version"]))

#' The putyourstyleguidehere style
#'
#' Style code according to the putyourstyleguidehere style guide. For more
#' details and docs, see the [styler::tidyverse_style()].
#' @inheritParams styler::tidyverse_style
#' @family obtain transformers
#' @family style_guides
#' @examples
#' style_text("call( 1)", scope = "spaces")
#' @importFrom purrr partial
#' @export
putyourstyleguidehere_style <- function(scope = "tokens",
                                        strict = TRUE,
                                        indent_by = 2,
                                        start_comments_with_one_space = FALSE,
                                        reindention = tidyverse_reindention(),
                                        math_token_spacing = tidyverse_math_token_spacing()) {
  args <- as.list(environment())
  scope <- styler:::scope_normalize(scope)
  indention_manipulators <- if ("indention" %in% scope) {
    list()
  }
  space_manipulators <- if ("spaces" %in% scope) {
    list()
  }

  use_raw_indention <- !("indention" %in% scope) || length(indention_manipulators) < 1

  line_break_manipulators <- if ("line_breaks" %in% scope) {
    list()
  }

  token_manipulators <- if ("tokens" %in% scope) {
    list(force_assignment_eq = force_assignment_eq)
  }



  create_style_guide(
    # transformer functions
    initialize = default_style_guide_attributes,
    line_break = line_break_manipulators,
    space = space_manipulators,
    indention = indention_manipulators,
    token = token_manipulators,
    # transformer options
    use_raw_indention = use_raw_indention,
    reindention = reindention,
    style_guide_name = "styler.putyourstyleguidehere::putyourstyleguidehere_style@https://github.com/putyourGitHubUserNameHere/styler.putyourstyleguidehere/",
    style_guide_version = version,
    more_specs_style_guide = args
  )
}
