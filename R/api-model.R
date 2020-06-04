


stringify_statements = function(code) {
  text = code %>% purrr::map( ~ rlang:::quo_get_expr(.x) %>% rlang::expr_text()) %>%
    purrr::map2(.x = names(.), .y = ., ~ dplyr::if_else(.x == "", paste0(.y, ";\n"), paste0(.x, " = ", .y, ";"))) 
  text = paste0(text, collapse = "\n") %>% paste0("\n\n")
  return(text)
}


