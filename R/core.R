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
  args = as.list(environment())
  scope = styler::scope_normalize(scope)
  indention_manipulators = if ("indention" %in% scope) {
    list()
  }
  space_manipulators = if ("spaces" %in% scope) {
    list()
  }

  use_raw_indention = !("indention" %in% scope) || length(indention_manipulators) < 1

  line_break_manipulators = if ("line_breaks" %in% scope) {
    list()
  }

  token_manipulators = if ("tokens" %in% scope) {
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
    style_guide_name = "styler.equals::equals_style@https://github.com/robinlovelace/styler.equals/",
    style_guide_version = version,
    more_specs_style_guide = args
  )
}

tidyverse_style <- function(scope = "tokens",
                            strict = TRUE,
                            indent_by = 2L,
                            start_comments_with_one_space = FALSE,
                            reindention = tidyverse_reindention(),
                            math_token_spacing = tidyverse_math_token_spacing()) {
  args <- as.list(environment())
  scope <- scope_normalize(scope)
  indent_character <- " "


  indention_manipulators <- if ("indention" %in% scope) {
    list(
      indent_braces = partial(indent_braces, indent_by = indent_by),
      unindent_fun_dec = unindent_fun_dec,
      indent_op = partial(indent_op, indent_by = indent_by),
      indent_eq_sub = partial(indent_eq_sub, indent_by = indent_by),
      indent_without_paren = partial(indent_without_paren,
                                     indent_by = indent_by
      ),
      update_indention_ref_fun_dec = update_indention_ref_fun_dec
    )
  }
  space_manipulators <- if ("spaces" %in% scope) {
    list(
      remove_space_before_closing_paren = remove_space_before_closing_paren,
      remove_space_before_opening_paren = if (strict) {
        remove_space_before_opening_paren
      },
      add_space_after_for_if_while = add_space_after_for_if_while,
      remove_space_before_comma = remove_space_before_comma,
      style_space_around_math_token = partial(
        style_space_around_math_token, strict,
        math_token_spacing$zero,
        math_token_spacing$one
      ),
      style_space_around_tilde = partial(
        style_space_around_tilde,
        strict = strict
      ),
      spacing_around_op = purrr::partial(set_space_around_op,
                                         strict = strict
      ),
      remove_space_after_opening_paren = remove_space_after_opening_paren,
      remove_space_after_excl = remove_space_after_excl,
      set_space_after_bang_bang = set_space_after_bang_bang,
      remove_space_before_dollar = remove_space_before_dollar,
      remove_space_after_fun_dec = remove_space_after_fun_dec,
      remove_space_around_colons = remove_space_around_colons,
      start_comments_with_space = partial(start_comments_with_space,
                                          force_one = start_comments_with_one_space
      ),
      remove_space_after_unary_pm_nested = remove_space_after_unary_pm_nested,
      spacing_before_comments = if (strict) {
        set_space_before_comments
      } else {
        add_space_before_comments
      },
      set_space_between_levels = set_space_between_levels,
      set_space_between_eq_sub_and_comma = set_space_between_eq_sub_and_comma,
      set_space_in_curly_curly = set_space_in_curly_curly
    )
  }

  use_raw_indention <- !("indention" %in% scope)

  line_break_manipulators <- if ("line_breaks" %in% scope) {
    list(
      set_line_break_around_comma_and_or = set_line_break_around_comma_and_or,
      set_line_break_after_assignment = set_line_break_after_assignment,
      set_line_break_before_curly_opening = set_line_break_before_curly_opening,
      remove_line_break_before_round_closing_after_curly =
        if (strict) remove_line_break_before_round_closing_after_curly,
      remove_line_breaks_in_fun_dec =
        if (strict) remove_line_breaks_in_fun_dec,
      style_line_break_around_curly = partial(
        style_line_break_around_curly,
        strict
      ),
      # must be after style_line_break_around_curly as it remove line
      # breaks again for {{.
      set_line_break_around_curly_curly = set_line_break_around_curly_curly,
      set_line_break_before_closing_call = if (strict) {
        partial(
          set_line_break_before_closing_call,
          except_token_before = "COMMENT"
        )
      },
      set_line_break_after_opening_if_call_is_multi_line = if (strict) {
        partial(
          set_line_break_after_opening_if_call_is_multi_line,
          except_token_after = "COMMENT",
          # don't modify line break here
          except_text_before = c("ifelse", "if_else"),
          force_text_before = "switch" # force line break after first token
        )
      },
      remove_line_break_in_fun_call = purrr::partial(
        remove_line_break_in_fun_call,
        strict = strict
      ),
      add_line_break_after_pipe = if (strict) add_line_break_after_pipe,
      set_line_break_after_ggplot2_plus = if (strict) {
        set_line_break_after_ggplot2_plus
      }
    )
  }

  token_manipulators <- if ("tokens" %in% scope) {
    list(
      fix_quotes = fix_quotes,
      force_assignment_op = force_assignment_op,
      resolve_semicolon = resolve_semicolon,
      add_brackets_in_pipe = add_brackets_in_pipe,
      wrap_if_else_while_for_fun_multi_line_in_curly =
        if (strict) {
          purrr::partial(
            wrap_if_else_while_for_fun_multi_line_in_curly,
            indent_by = indent_by
          )
        }
    )
  }

  transformers_drop <- specify_transformers_drop(
    spaces = list(
      # remove_space_before_closing_paren = c("')'", "']'"),
      # remove_space_before_opening_paren = c("'('", "'['", "LBB"),
      add_space_after_for_if_while = c("IF", "WHILE", "FOR"),
      # remove_space_before_comma = "','",
      set_space_between_eq_sub_and_comma = "EQ_SUB",
      style_space_around_math_token = c(
        math_token_spacing$zero,
        math_token_spacing$one
      ),
      style_space_around_tilde = "'~'",
      # remove_space_after_opening_paren = c("'('", "'['", "LBB"),
      remove_space_after_excl = "'!'",
      set_space_after_bang_bang = "'!'",
      remove_space_before_dollar = "'$'",
      remove_space_after_fun_dec = "FUNCTION",
      remove_space_around_colons = c("':'", "NS_GET_INT", "NS_GET"),
      start_comments_with_space = "COMMENT",
      remove_space_after_unary_pm_nested = c("'+'", "'-'"),
      spacing_before_comments = "COMMENT",
      set_space_in_curly_curly = c("'{'", "'}'")
    ),
    indention = list(
      # indent_braces = c("'('", "'['", "'{'", "')'", "']'", "'}'"),
      unindent_fun_dec = "FUNCTION",
      indent_eq_sub = c("EQ_SUB", "EQ_FORMALS"), # TODO rename
      update_indention_ref_fun_dec = "FUNCTION"
    ),
    line_breaks = list(
      set_line_break_before_curly_opening = "'{'",
      remove_line_break_before_round_closing_after_curly = "'}'",
      remove_line_breaks_in_fun_dec = "FUNCTION",
      set_line_break_around_curly_curly = "'{'",
      style_line_break_around_curly = "'{'",
      add_line_break_after_pipe = c("SPECIAL-PIPE", "PIPE")
    ),
    tokens = list(
      resolve_semicolon = "';'",
      add_brackets_in_pipe = c("SPECIAL-PIPE", "PIPE"),
      # before 3.6, these assignments are not wrapped into top-level expression
      # and `text` supplied to transformers_drop() is "", so it appears to not
      # contain EQ_ASSIGN, and the transformer is falsely removed.
      # compute_parse_data_nested / text_to_flat_pd ('a = 4')
      force_assignment_op = "EQ_ASSIGN",
      wrap_if_else_while_for_fun_multi_line_in_curly = c(
        "IF", "WHILE", "FOR", "FUNCTION"
      )
    )
  )

  style_guide_name <- "styler::tidyverse_style@https://github.com/r-lib"
  create_style_guide(
    # transformer functions
    initialize             = default_style_guide_attributes,
    line_break             =        line_break_manipulators,
    space                  =             space_manipulators,
    token                  =             token_manipulators,
    indention              =         indention_manipulators,
    # transformer options
    use_raw_indention      =              use_raw_indention,
    reindention            =                    reindention,
    style_guide_name       =               style_guide_name,
    style_guide_version    =                 styler_version,
    more_specs_style_guide =                           args,
    transformers_drop      =              transformers_drop,
    indent_character       =               indent_character
  )
}
