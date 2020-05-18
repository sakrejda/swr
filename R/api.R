
#' @export
stringify_declaration = function(x) {
  if ("Transform" %in% class(x)) {
    o = x$textify()
  } else if ("BareType" %in% class(x)) {
    o = x$textify()
  } else if (rlang::is_symbol(x)) {
    o = as.character(x)
  } else if (rlang::is_syntactic_literal(x)) {
    o = as.character(x)
  } else if (rlang::is_call(x)) {
    o = purrr::map(x, stringify)
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
`[.MyClass` = function(e1, e2) e1$get(e2)
