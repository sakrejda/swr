
#' Bare type for anything that represents a block of Stan code
BareBlockType = R6::R6Class("BareBlockType")

#' Check that 'x' is a Stan block
#'
#' @param x object to check
#' @return TRUE iff 'x' is a BlockType
#' @export
is_block = function(x) "BareBlockType" %in% class(x)

#' Check that 'x' is NOT a Stan block
#'
#' @param x object to check
#' @return TRUE iff 'x' is NOT a BlockType
#' @export
is_not_block = function(x) !is_block(x)

#' Check that 'x' is an inline Stan block
#'
#' @param x object to check
#' @return TRUE iff 'x' is a InlineBlockType
#' @export
is_block = function(x) "InlineBlockType" %in% class(x)


#' An inline block of Stan code
#'
#' An inline block has a textify method for the whole block, but also handles
#' nested blocks (which MultiStatementType does not!)
InlineBlockType = R6::R6Class("InlineBlockType", inherit = BareBlockType,
  public = list(
    initialize = function(..., declarations = rlang::env(), indent = 0) {
      private$statements_ = list()
      private$declarations_ = declarations
      statements = wrap_statements(..., declarations = declarations)
      for (i in seq_along(statements)) {
        self$append(statements[[i]])
      }
    },
    textify_declarations = function() {
      decl = list()
      for (i in seq_along(private$statements_)) {
        if (!is_block(private$statements_[[i]])) {
          decl = c(decl, stringify_declarations(private$statements_[i], private$declarations_))
        } else {
          # nested blocks do not generate declarations, they have their own!
        }
      }
      decl = unique(decl)
      return(decl)
    },
    textify_statements = function() {
      stmts = list()
      for (i in seq_along(private$statements_)) {
        if (is_block(private$statements_[[i]])) {
          stmts = c(stmts, private$statements_[[i]]$textify())
        } else {
          stmts = c(stmts, stringify_statements(private$statements_[i]))
        }
      }
      return(stmts)
    },
    textify = function() {
      o = c(self$textify_declarations(), self$textify_statements())
      return(o)
    },
    indent = function() private$indent_ = private$indent_ + 0,
    statements = function() return(private$statements_),
    declarations = function() return(private$declarations_),
    append = function(x) { 
      if (isTRUE("SingleStatementType" %in% class(x))) {
        private$statements_ = c(private$statements_, x)
        swr:::env_copy(x$declarations(), private$declarations_, nms = ls(x$declarations()))
      }
      if (isTRUE("MultiStatementType" %in% class(x))) {
        statements = x$statements()
        for (i in seq_along(statements)) {
          self$append(statements[[i]])
        }
        swr:::env_copy(x$declarations(), private$declarations_, nms = ls(x$declarations()))
      }
      if (is_block(x)) {
        x$indent()
        private$statements_ = c(private$statements_, x) 
      }
    }
  ),
  private = list(
    statements_ = list(),
    declarations_ = rlang::env(),
    indent_ = 0
  )
)


NamedBlockType = R6::R6Class("NamedBlockType", inherit = InlineBlockType, 
  public = list(
    initialize = function(name = "", ..., declarations = rlang::env()) {
      private$block_name_ = name
      super$initialize(..., declarations = declarations)
    },
    textify = function() {
      if (private$block_name_ != "") {
        s = c(paste(private$block_name_, "{"), super$textify(), "}")
      } else {
        s = c("{", super$textify(), "}")
      }
      return(s)
    }
  ),
  private = list(
    block_name_ = ""
  )
)

AnonymousBlockType = R6::R6Class("AnonymousBlockType", inherit = NamedBlockType, 
  public = list(
    initialize = function(..., declarations = rlang::env()) {
      super$initialize(name = "", ..., declarations = declarations)
    }
  )
)


DataBlockType = R6::R6Class("DataBlockType", inherit = NamedBlockType,
  public = list(
    initialize = function(..., declarations = rlang::env()) {
      super$initialize("data", ..., declarations = declarations)
    }
  )
)

TransformedDataBlockType = R6::R6Class("TransformedDataBlockType", inherit = NamedBlockType,
  public = list(
    initialize = function(..., declarations = rlang::env()) {
      super$initialize("transformed data", ..., declarations = declarations)
    }
  )
)

ParametersBlockType = R6::R6Class("ParametersBlockType", inherit = NamedBlockType,
  public = list(
    initialize = function(..., declarations = rlang::env()) {
      super$initialize("parameteres", ..., declarations = declarations)
    }
  )
)

TransformedParametersBlockType = R6::R6Class("TransformedParametersBlockType", inherit = NamedBlockType,
  public = list(
    initialize = function(..., declarations = rlang::env()) {
      super$initialize("transformed parameters", ..., declarations = declarations)
    }
  )
)

ModelBlockType = R6::R6Class("ModelBlockType", inherit = NamedBlockType,
  public = list(
    initialize = function(..., declarations = rlang::env()) {
      super$initialize("data", ..., declarations = declarations)
    }
  )
)

