#' Force the assignment operator `=` when possible
#'
#' Because you can't determine from a nest with `<-` what the context is,
#' we need to do it from the parent.
#' @keywords internal
force_assignment_eq = function(pd) {
  if (styler::is_function_call(pd)) {
    return(pd)
  }
  before_assignemnt = styler::previous_non_comment(
    pd,
    which(pd$token == "expr")[1]
  )

  # don't replace if before is (
  if (length(before_assignemnt) > 0 && pd$token[before_assignemnt] == "'('") {
    return(pd)
  }
  pd$child[pd$token == "expr"] = purrr::map(
    pd$child[pd$token == "expr"], force_assignment_eq_impl
  )
  pd
}

force_assignment_eq_impl = function(pd_child) {
  to_replace = pd_child$token == "LEFT_ASSIGN" & pd_child$text == "<-"
  if (any(to_replace)) {
    pd_child$token[to_replace] = "EQ_ASSIGN"
    pd_child$text[to_replace] = "="
  }
  return(pd_child)
}
