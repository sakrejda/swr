
#' @export
stringify = function(x) {
  if ("Transform" %in% class(x)) {
    o = x$textify()
  } else if ("BareType" %in% class(x)) {
    o = x$textify()
  } else if ("BareStatementType" %in% class(x)) {
    o = x$textify()
  } else if (rlang::is_symbol(x)) {
    o = as.character(x)
  } else if (rlang::is_syntactic_literal(x)) {
    o = as.character(x)
  } else if (rlang::is_call(x)) {
    o = rlang::expr_text(x, width = 80) %>%
      substr(2, nchar(.))
  } else {
    stop("Failed to stringify")
  }
  return(o)
}

#' @export
set = function(.data, x) {
  # FIXME: needs dimension checks!
  x_ = rlang::enquo(x)
  .data$set_value(!!x_) 
  return(.data)
}

#' @export
`[.CoreType` = function(e1, ...) e1$get(...)
