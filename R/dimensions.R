
#' @export
Dimensions = R6::R6Class("Dimensions",
  public = list(
    initialize = function(...) {
      dims = rlang::enquos(...)
      private$ndims_ = length(dims)
      if (!identical(rlang::enquos(), dims)) {
        private$dims_ = dims
      }
    },
    textify = function() {
      if (private$ndims_ == 0)
        return(NULL)
      s = purrr::map(private$dims_, ~ stringify(.x))
      s = paste0(s, collapse = ', ')
      s = paste0("[", s, "]")
      return(s)
    }
  ),
  private = list(
    ndims_ = 0,
    dims_ = rlang::quos()
  ),
  active = list(
    ndims = function() return(private$ndims_)
  )
)
