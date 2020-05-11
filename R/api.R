

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
dimensions = function(...) swr::Dimensions$new(...)

#' @export 
int = function(
  name, 
  transforms = swr::transforms(), 
  dimensions = swr::dimensions(),
  value = NULL
) {
  val = rlang::enquo(value)
  o = swr::IntType$new(name, transforms, dimensions, !!val)
  return(o)
}

#' @export 
real = function(
  name, 
  transforms = swr::transforms(), 
  dimensions = swr::dimensions(),
  value = NULL
) {
  val = rlang::enquo(value)
  o = swr::RealType$new(name, transforms, dimensions, !!val)
  return(o)
}

#' @export 
vector = function(
  name, 
  transforms = swr::transforms(), 
  size = swr::dimensions(0),
  dimensions = swr::dimensions(),
  value = NULL
) {
  val = rlang::enquo(value)
  if (size$ndims != 1) {
    stop("A vector's size must be a scalar.")
  }
  o = swr::VectorType$new(name, transforms, size, dimensions, !!val)
  return(o)
}

#' @export 
row_vector = function(
  name, 
  transforms = swr::transforms(), 
  size = swr::dimensions(0),
  dimensions = swr::dimensions(),
  value = NULL
) {
  val = rlang::enquo(value)
  if (size$ndims != 1) {
    stop("A row_vector's size must be a scalar.")
  }
  o = swr::RowVectorType$new(name, transforms, size, dimensions, !!val)
  return(o)
}


#' @export
set = function(.data, x) {
  # FIXME: needs dimension checks!
  x_ = rlang::enquo(x)
  .data$set_value(!!x_) 
  return(.data)
}



