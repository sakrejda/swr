


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
  o = swr::RowVectorType$new(name, transforms, size, dimensions, !!val)
  return(o)
}

#' @export 
simplex = function(
  name, 
  transforms = swr::transforms(), 
  size = swr::dimensions(0),
  dimensions = swr::dimensions(),
  value = NULL
) {
  val = rlang::enquo(value)
  o = swr::SimplexType$new(name, transforms, size, dimensions, !!val)
  return(o)
}

#' @export 
unit_vector = function(
  name, 
  transforms = swr::transforms(), 
  size = swr::dimensions(0),
  dimensions = swr::dimensions(),
  value = NULL
) {
  val = rlang::enquo(value)
  o = swr::UnitVectorType$new(name, transforms, size, dimensions, !!val)
  return(o)
}

#' @export 
ordered = function(
  name, 
  transforms = swr::transforms(), 
  size = swr::dimensions(0),
  dimensions = swr::dimensions(),
  value = NULL
) {
  val = rlang::enquo(value)
  o = swr::OrderedType$new(name, transforms, size, dimensions, !!val)
  return(o)
}

#' @export 
positive_ordered = function(
  name, 
  transforms = swr::transforms(), 
  size = swr::dimensions(0),
  dimensions = swr::dimensions(),
  value = NULL
) {
  val = rlang::enquo(value)
  o = swr::PositiveOrderedType$new(name, transforms, size, dimensions, !!val)
  return(o)
}

#' @export 
matrix = function(
  name, 
  transforms = swr::transforms(), 
  size = swr::dimensions(0, 0),
  dimensions = swr::dimensions(),
  value = NULL
) {
  val = rlang::enquo(value)
  o = swr::MatrixType$new(name, transforms, size, dimensions, !!val)
  return(o)
}

#' @export 
cov_matrix = function(
  name, 
  transforms = swr::transforms(), 
  size = swr::dimensions(0),
  dimensions = swr::dimensions(),
  value = NULL
) {
  val = rlang::enquo(value)
  o = swr::CovMatrixType$new(name, transforms, size, dimensions, !!val)
  return(o)
}

#' @export 
corr_matrix = function(
  name, 
  transforms = swr::transforms(), 
  size = swr::dimensions(0),
  dimensions = swr::dimensions(),
  value = NULL
) {
  val = rlang::enquo(value)
  o = swr::CorrMatrixType$new(name, transforms, size, dimensions, !!val)
  return(o)
}

#' @export 
cholesky_factor_cov = function(
  name, 
  transforms = swr::transforms(), 
  size = swr::dimensions(0, 0),
  dimensions = swr::dimensions(),
  value = NULL
) {
  val = rlang::enquo(value)
  o = swr::CholeskyFactorCovType$new(name, transforms, size, dimensions, !!val)
  return(o)
}
