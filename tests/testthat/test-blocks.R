test_that("we can generate a block of statements", {
  e1 = rlang::env()
  e1$a = swr::vector("a", size = swr:::dimensions(3 * Z), value = erm / 5)
  e1$b = swr::simplex("b", size = swr:::dimensions(3 * Z), value = urkh - 3) 
  e1$c = swr::real("c", value = 7 * G) 
  e1$g = swr::real("g", value = pi())
  inline_block = swr:::InlineBlockType$new(
    a = b,
    c = dot_product(a, b),
    Q = b * (g + c),
    declarations = e1)
  inline_check = list(
    "simplex[3 * Z] b = urkh - 3;",
    "vector[3 * Z] a = erm/5;",
    "real g = pi();",
    "real c = 7 * G;",
    "a = b;",
    "c = dot_product(a, b);",
    "Q = b * (g + c);"
  )
  testthat::expect_equal(inline_block$textify(), inline_check)
})

test_that("we can generate an inline block.", {
  e1 = rlang::env()
  p = 0.35
  N = 70
  K = 15
  y = rbinom(n = N, size = K, prob = p)
  e1$N = swr::int("N", value = N)
  e1$K = swr::int("K", value = K)
  e1$y = swr::int("y", dimensions = swr:::dimensions(N), value = y)
  inline_block = swr:::InlineBlockType$new(
    target %+=% binomial_lpmf(y | N, p),
    declarations = e1)
  inline_block$textify() %>% purrr::lift_dl(cat, sep = "\n")()
}




