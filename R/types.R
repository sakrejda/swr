
BareType = R6::R6Class("BareType")

CoreType = R6::R6Class("CoreType", inherit = BareType,
  public = list(
    initialize = function(name = NULL, type = NULL, transforms = NULL, size = NULL, dimensions = NULL, value = NULL) {
      if (is.null(name)) {
        stop("Every declared type must have a name.")
      } else {
        private$name_ = name
      }
      if (is.null(type)) {
        stop("Every declared type must have a specific type.")
      } else {
        private$type_ = type
      }
      if (!is.null(transforms)) {
        private$transforms_ = transforms
      }
      if (!is.null(size)) {
        private$size_ = size
      }
      if (!is.null(dimensions)) {
        private$dimensions_ = dimensions
      }
      private$value_ = rlang::enquo(value)
    },
    textify = function() { 
      s = paste0(private$type_, private$transforms_$textify())
      s = paste0(s, private$size_$textify())
      s = paste0(s, " ", private$name_)
      s = paste0(s, private$dimensions_$textify())
      if (!identical(rlang::quo(), private$value_) && 
          !identical(rlang::quo(NULL), private$value_)) {
        s = paste0(s, " = ", string_order(stringify(private$value_)))
      }
      s = paste0(s, ";")
      return(s)
    },
    set_value = function(x) {
      private$value_ = rlang::enquo(x)
      return(self)
    }
  ),
  private = list(
    name_ = "",
    type_ = "",
    transforms_ = Transforms$new(),
    size_ = Dimensions$new(),
    dimensions_ = Dimensions$new(),
    value_ = rlang::quo()
  )
)


#' @export
IntType = R6::R6Class("IntType", inherit = CoreType,
  public = list(
    initialize = function(name, transforms = swr::transforms(), dimensions = swr::dimensions(), value = NULL) {
      val = rlang::enquo(value)
      dim = rlang::enquo(dimensions)
      super$initialize(name, "int", transforms, dimensions = dimensions, value = !!val)
    }
  )
)

#' @export
RealType = R6::R6Class("RealType", inherit = CoreType,
  public = list(
    initialize = function(name, transforms = swr::transforms(), dimensions = swr::dimensions(), value = NULL) {
      val = rlang::enquo(value)
      dim = rlang::enquo(dimensions)
      super$initialize(name, "real", transforms, dimensions = dimensions, value = !!val)
    }
  )
)

#' @export
VectorType = R6::R6Class("VectorType", inherit = CoreType,
  public = list(
    initialize = function(name, transforms = swr::transforms(), size = swr::dimensions(0), dimensions = swr::dimensions(), value = NULL) {
      val = rlang::enquo(value)
      super$initialize(name, "vector", transforms, size, dimensions, value = !!val)
    }
  )
)

#' @export
RowVectorType = R6::R6Class("RowVectorType", inherit = CoreType,
  public = list(
    initialize = function(name, transforms = swr::transforms(), size = swr::dimensions(0), dimensions = swr::dimensions(), value = NULL) {
      val = rlang::enquo(value)
      super$initialize(name, "row_vector", transforms, size, dimensions, value = !!val)
    }
  )
)


