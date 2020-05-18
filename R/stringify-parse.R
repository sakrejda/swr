

unary_prefix_op = function() c('!', '-')
binary_infix_op = function() c('+', '-', '*', '/')
unary_postfix_op = function() c('[')

#' @export
string_order = function(s) {
  if (length(s) == 1) {
    return(s)
  } else if (s[[1]] == '~') {
    s = string_order(s[[2]])
    return(s)
  } else if (s[[1]] %in% unary_prefix_op() && length(s) == 2) {
    s = paste0(string_order(s[[1]]), string_order(s[[2]]))
    return(s)
  } else if (s[[1]] %in% binary_infix_op()) {
    s = paste(string_order(s[[2]]), s[[1]], string_order(s[[3]]))
    return(s)
  } else if (s[[1]] %in% unary_postfix_op()) {
    if (length(s[[2]]) == 1) {
      s = paste0(s[[2]], "[", paste0(purrr::map(s[3:length(s)], string_order), collapse = ", "), "]")
    } else {
      s = paste0("(", s[[2]], ")[", paste0(purrr::map(s[3:length(s)], string_order), collapse = ", "), "]")
    }
    return(s)
  }
  s = paste0(s[[1]], "(", paste0(purrr::map(s[2:length(s)], string_order), collapse = ", "), ")")
  return(s)
}

textify.numeric = function(x, .hint = "vector") {
  enq = rlang::enquo(x)
  v = x
  k = length(x)
  if (.hint == "vector") {
  } else if (.hint == "array") {

  }
}
