

#' @export
transforms = function(
  lb = -Inf,
  ub = Inf,
  offset = 0,
  scale = 1
) {

  lb = LowerBound$new(!!rlang::enquo(lb))
  ub = UpperBound$new(!!rlang::enquo(ub))
  offset = Offset$new(!!rlang::enquo(offset))
  scale = Scale$new(!!rlang::enquo(scale))
  trf = Transforms$new(lb, ub, offset, scale)
  return(trf)
}

#' @export
int = function(name, transforms = swr::transforms(), value = NULL) {
  val = rlang::enquo(value)
  o = swr::IntType$new(name, transforms, !!val)
  return(o)
}

