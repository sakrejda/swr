

#' @export
transforms = function(
  lb = -Inf,
  ub = Inf,
  offset = 0,
  scale = 1
) {
  if (!identical(-Inf, lb)) {
    lb = LowerBound$new(!!rlang::enquo(lb))
  } else {
    lb = NULL
  }
  if (!identical(Inf, ub)) {
    ub = UpperBound$new(!!rlang::enquo(ub))
  } else {
    ub = NULL
  }
  if (!identical(0, offset)) {
    offset = Offset$new(!!rlang::enquo(offset))
  } else {
    offset = NULL
  }
  if (!identical(1, scale)) {
    scale = Scale$new(!!rlang::enquo(scale))
  } else {
    scale = NULL
  }
  trf = Transforms$new(lb, ub, offset, scale)
  return(trf)
}

#' @export
int = function(name, transforms = swr::transforms(), value = NULL) {
  val = rlang::enquo(value)
  o = swr::IntType$new(name, transforms, !!val)
  return(o)
}

#' @export
real = function(name, transforms = swr::transforms(), value = NULL) {
  val = rlang::enquo(value)
  o = swr::RealType$new(name, transforms, !!val)
  return(o)
}
