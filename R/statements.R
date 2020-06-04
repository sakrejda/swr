
pick_symbols = function(x) {
  if (rlang::is_symbol(x)) {
    return(as.character(x)) 
  } else if (rlang::is_syntactic_literal(x)) {
    return(NULL)
  } else if (rlang::is_call(x) || rlang::is_quosures(x)) {
    return(purrr::map(x, pick_symbols))
  }
}

pick_variables = function(code, env) {
  symbols = code %>% purrr::map(pick_symbols) %>% unlist()
  vars = purrr::map(symbols, ~ rlang::env_get(env, .x, default = NULL)) %>% 
    rlang::set_names(symbols) %>% purrr::compact()
  return(vars)
}

stringify_declaration = function(code, env) {
  vars = pick_variables(code, env)
  text = purrr::map(vars, stringify)
  return(text)
}

stringify_declarations = function(code, env) {
  text = purrr::map(code, stringify_declaration)
  return(text)
}

stringify_statements = function(code) {
  text = code %>% purrr::map( ~ rlang:::quo_get_expr(.x) %>% rlang::expr_text()) %>%
    purrr::map2(.x = names(.), .y = ., ~ dplyr::if_else(.x == "", paste0(.y, ";"), paste0(.x, " = ", .y, ";"))) 
  return(text)
}


BareBlockType = R6::R6Class("BareBlockType")

InlineBlockType = R6::R6Class("InlineBlockType", inherit = "BareBlockType",
  public = list(
    initialize = function(..., declarations = rlang::env()) {
      private$statements_ = rlang::enquos(...)
      private$declarations_ = declarations
    },
    textify = function() {
      s = c(stringify_declarations(private$statements_),
        stringify_statements(private$statements_))
      return(s)
    }
  ),
  private = list(
    statements_ = rlang::enquos(),
    declarations_ = rlang::env()
  )
)

NamedBlockType = R6::R6Class(inherit = "InlineBlockType", 
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

DataBlockType = R6::R6Class(inherit = "NamedBlockType",
  public = list(
    initialize = function(..., declarations = rlang::env()) {
      super$initialize("data", ..., declarations = declarations)
    }
  )
)

TransformedDataBlockType = R6::R6Class(inherit = "NamedBlockType",
  public = list(
    initialize = function(..., declarations = rlang::env()) {
      super$initialize("transformed data", ..., declarations = declarations)
    }
  )
)

ParametersBlockType = R6::R6Class(inherit = "NamedBlockType",
  public = list(
    initialize = function(..., declarations = rlang::env()) {
      super$initialize("parameteres", ..., declarations = declarations)
    }
  )
)

TransformedParametersBlockType = R6::R6Class(inherit = "NamedBlockType",
  public = list(
    initialize = function(..., declarations = rlang::env()) {
      super$initialize("transformed parameters", ..., declarations = declarations)
    }
  )
)

ModelBlockType = R6::R6Class(inherit = "NamedBlockType",
  public = list(
    initialize = function(..., declarations = rlang::env()) {
      super$initialize("data", ..., declarations = declarations)
    }
  )
)

