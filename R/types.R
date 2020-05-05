
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
      s = paste0(private$type_, private$transforms_$textify(), " ", private$name_)
      if (!identical(rlang::quo(NULL), private$value_)) {
        s = paste0(s, " = ", string_order(stringify(private$value_)), ";")
      }
      return(s)
    }
  ),
  private = list(
    name_ = "",
    type_ = "",
    transforms_ = Transforms$new(),
    value_ = rlang::quo()
  )
)

#' @export
IntType = R6::R6Class("IntType", inherit = CoreType,
  public = list(
    initialize = function(name, transforms, value = NULL) {
      val = rlang::enquo(value)
      super$initialize(name, "int", transforms, value = !!val)
    }
  )
)

#' @export
RealType = R6::R6Class("RealType", inherit = CoreType,
  public = list(
    initialize = function(name, transforms, value = NULL) {
      val = rlang::enquo(value)
      super$initialize(name, "real", transforms, value = !!val)
    }
  )
)

