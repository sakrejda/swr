
#' Extract all symbols from an expression as a tree strings
#'
#' The leading guard ("~") is extracted, parenthesis order is 
#' retained but parentheses are not.
#'
#' @param x expression to extract symbols from
#' @return a tree (nested lists) of character vectors
#' @export
pick_symbols = function(x) {
  if (rlang::is_symbol(x)) {
    return(as.character(x)) 
  } else if (rlang::is_syntactic_literal(x)) {
    return(NULL)
  } else if (rlang::is_call(x) || rlang::is_quosures(x)) {
    return(purrr::map(x, pick_symbols))
  }
}

#' Extract all variables used in the code that exist in the environment
#'
#' Uses `pick_symbols(code)` followed by filtering, undefined symbols are not
#' returned.
#'
#' @param code list of expressions to extract variables from
#' @param env environment to check for variables
#' @return a named list of the variables and their values as defined
#' @export
pick_variables = function(code, env) {
  symbols = code %>% purrr::map(pick_symbols) %>% unlist() %>% unique
  vars = purrr::map(symbols, ~ rlang::env_get(env, .x, default = NULL)) %>% 
    rlang::set_names(symbols) %>% purrr::compact()
  return(vars)
}

#' Transform list of expressions into a list of Stan variable declarations
#'
#' @param code list of expressions to extract variables from
#' @param env environment to check for variables
#' @return a named list of the variables and their corresponding Stan
#'         declarations
#' @export
stringify_declarations = function(code, env) {
  vars = pick_variables(code, env)
  text = purrr::map(vars, stringify)
  return(text)
}

#' Transform a list of expressions into a list of Stan code statements
#'
#' @param code list of expressions to extract variables from
#' @return a named list of the variables and their corresponding Stan
#'         declarations
#' @export
stringify_statements = function(code) {
  text = code %>% purrr::map( ~ rlang:::quo_get_expr(.x) %>% rlang::expr_text()) %>%
    purrr::map2(.x = names(.), .y = ., ~ dplyr::if_else(.x == "", paste0(.y, ";"), paste0(.x, " = ", .y, ";"))) 
  return(text)
}

#' Placeholder for anything that is a Stan statement
BareStatementType = R6::R6Class("BareStatementType")

#' Check that 'x' is a Stan statement
#'
#' @param x object to check
#' @return TRUE iff 'x' is a StatementType
#' @export
is_statement = function(x) "BareStatementType" %in% class(x)

is_single_statement = function(x) 'SingleStatementType' %in% class(x)
is_multi_statement = function(x) 'MultiStatementType' %in% class(x)

#' Check that 'x' is NOT a Stan statement
#'
#' @param x object to check
#' @return TRUE iff 'x' is NOT a StatementType
#' @export
is_not_statement = function(x) !is_statement(x)

#' Class representing a single Stan statement, not user-facing
#'
#' @param x expression to treat as Stan statement
#' @param declarations environment to use to create declarations
#' @return class representing the statement
#' @export
SingleStatementType = R6::R6Class("SingleStatementType", inherit = BareStatementType,
  public = list(
    initialize = function(x, declarations = rlang::env()) {
      private$statements_ = rlang::enquo(x)
      private$declarations_ = declarations
    },
    textify_declarations = function() {
      x = stringify_declarations(c(rlang::quos(), private$statements_), private$declarations_)
      return(x)
    },
    textify_statements = function() {
      x = stringify_statements(c(rlang::quos(), private$statements_))
      return(x)
    },
    statements = function() return(private$statements_),
    declarations = function() return(private$declarations_)
  ),
  private = list(
    statements_ = rlang::enquo(),
    declarations_ = rlang::env()
  )
)

#' Class representing a series of Stan statements, not user-facing
#'
#' @param ... expressions to treat as Stan statements
#' @param declarations environment to use to create declarations
#' @return class representing the statement
#' @export
MultiStatementType = R6::R6Class("MultiStatementType", inherit = BareStatementType,
  public = list(
    initialize = function(..., declarations = rlang::env()) {
      private$statements_ = list()
      private$declarations_ = declarations
      statements = wrap_statements(..., declarations = declarations)
      for (i in seq_along(statements)) {
        self$append(statements[[i]])
      }
    },
    textify_declarations = function() {
      x = purrr::map(private$statements_, ~ .x$textify_declarations()) %>% 
        purrr::flatten() %>% unique()
      return(x)
    },
    textify_statements = function() {
      x = purrr::map(private$statements_, ~ .x$textify_statements()) %>%
        purrr::flatten()
      return(x)
    },
    textify = function() {
      decls = self$textify_declarations()
      stmts = self$textify_statements()
      snippet = c(decls, stmts)
    },
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
    }
  ),
  private = list(
    statements_ = list(),
    declarations_ = rlang::env()
  )
)

#' Wrap expressions (or statements) into a list of statements
#'
#' Works even if the expressions can not be evaluated
#'
#' @param ... any expressions or anything descended from BareStatementType
#' @param declarations environment containing declared Stan variables
#' @return 
#' @export
wrap_statements = function(..., declarations = rlang::env()) {
  args = rlang::enquos(...); 
  o = list(); 
  for (i in seq_along(args)) {
    expr = rlang::quo_get_expr(args[[i]])
    err_cl = class(rlang::catch_cnd(rlang::eval_tidy(expr))); 
    can_eval = !isTRUE('error' %in% err_cl); 
    if (can_eval) {
      if (is_statement(rlang::eval_tidy(expr))) {
        o[[i]] = rlang::eval_tidy(expr)
      } else if (is_block(rlang::eval_tidy(expr))) {
        o[[i]] = rlang::eval_tidy(expr)
      } else {
        o[[i]] = SingleStatementType$new(!!expr, declarations)
      }
    } else {
      o[[i]] = SingleStatementType$new(!!expr, declarations)
    }
  }
  return(o)
}


