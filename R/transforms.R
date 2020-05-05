
Transform = R6::R6Class("Transform")

#' @export
LowerBound = R6::R6Class("LowerBound", inherit = Transform,
  public = list(
    initialize = function(x) {
      if (!missing(x)) {
        private$lb_ = rlang::enquo(x)
      }
    },
    textify = function() {
      s = private$lb_ %>% stringify %>% string_order
      s = paste0("lower=", s)
      return(s)
    },
    value = function() {
      return(private$lb_)
    }
  ), 
  private = list(
    lb_ = rlang::quo(-Inf)
  )
)

#' @export
UpperBound = R6::R6Class("LowerBound",  inherit = Transform,
  public = list(
    initialize = function(x) {
      if (!missing(x)) {
        private$ub_ = rlang::enquo(x)
      }
    },
    textify = function() {
      s = private$ub_ %>% stringify %>% string_order
      s = paste0("upper=", s)
      return(s)
    },
    value = function() {
      return(private$ub_)
    }
  ), 
  private = list(
    ub_ = rlang::quo(Inf)
  )
)

#' @export
Offset = R6::R6Class("Offset", inherit = Transform,
  public = list(
    initialize = function(x) {
      if (!missing(x)) {
        private$offset_ = rlang::enquo(x)
      }
    },
    textify = function() {
      s = private$offset_ %>% stringify %>% string_order
      s = paste0("offset=", s)
      return(s)
    },
    value = function() {
      return(private$offset_)
    }
  ), 
  private = list(
    offset_ = rlang::quo(0)
  )
)

#' @export
Scale = R6::R6Class("Scale", inherit = Transform,
  public = list(
    initialize = function(x) {
      if (!missing(x)) {
        private$scale_ = rlang::enquo(x)
      }
    },
    textify = function() {
      s = private$scale_ %>% stringify %>% string_order
      s = paste0("scale=", s)
      return(s)
    },
    value = function() {
      return(private$scale_)
    }
  ), 
  private = list(
    scale_ = rlang::quo(1)
  )
)

#' @export
Unspecified = R6::R6Class("Unspecified", inherit = Transform,
  public = list(
    initialize = function(...) {},
    textify = function() return(NULL)
  )
)

#' @export
Transforms = R6::R6Class("Transforms",
  public = list(
    initialize = function(lb, ub, offset, scale) {
      if (!missing(lb)) {
        private$lb_ = lb
      } 
      if (!missing(ub)) {
        private$ub_ = ub
      }
      if (!missing(offset)) {
        private$offset_ = offset
      } 
      if (!missing(scale)) {
        private$scale_ = scale
      }
    },
    textify = function() {
      s = c(private$lb_$textify(), private$ub_$textify(),
                 private$offset_$textify(), private$scale_$textify())
      s = paste0("<", purrr::lift_dl(paste)(s, sep = ", "), ">")
      if (s != '<>') {
        return(s)
      } else {
        return(NULL)
      }
    }
  ),
  private = list(
    lb_ = Unspecified$new(),
    ub_ = Unspecified$new(),
    offset_ = Unspecified$new(),
    scale_ = Unspecified$new()
  )
)

