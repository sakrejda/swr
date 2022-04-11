
test_that("we can stringify an expression", {
  o = rlang::quo(a + b * -c + f(g))
  s = swr::stringify(o)
  testthat::expect_equal(s, "a + b * -c + f(g)")
})

test_that("we can extract the symbols from an expression", {
  o = rlang::quo(a + b * -c + f(g))
  sym = swr::pick_symbols(o)
  check = list("~",
    list(
      "+",
      list(
        "+",
        "a",
        list(
          "*",
          "b",
          list("-", "c")
        )
      ),
      list("f", "g")
    ))
  testthat::expect_equal(sym, check)
})

test_that("we can extract variables defined in an environment and used in an expression", {
  o = rlang::quo(a + b * -c + f(g))
  e1 = rlang::env()
  sym1 = swr::pick_variables(o, e1)
  testthat::expect_equal(length(sym1), 0)
  e1$a = 33
  e1$b = 44
  e1$g = 99
  sym2 = swr::pick_variables(o, e1)
  check1 = list(a = 33, b = 44, g = 99)
  testthat::expect_equal(sym2, check1)
  e1$c = 55  
  check2 = list(a = 33, b = 44, c = 55, g = 99)
  sym3 = swr::pick_variables(o, e1)
  testthat::expect_equal(sym3, check2)
})

test_that("we can turn an expression into variable delcarations", {
  o = rlang::quo(a + b * -c + f(g))
  e1 = rlang::env()
  e1$a = swr::vector("a", size = swr:::dimensions(3 * Z), value = erm / 5)
  e1$b = swr::simplex("b", size = swr:::dimensions(3 * Z), value = urkh - 3) 
  e1$c = swr::real("c", value = 7 * G) 
  e1$g = swr::real("g", value = pi())
  decl = swr::stringify_declarations(o, e1)
  decl_known = list(
    a = "vector[3 * Z] a = erm/5;",
    b = "simplex[3 * Z] b = urkh - 3;",
    c = "real c = 7 * G;",
    g = "real g = pi();"
  )
  testthat::expect_equal(decl, decl_known)
})

test_that("we can turn a list of expressions into a list of Stan statements", {
  o = rlang::quos(A = 33, B = 57, mu = A + B, sd = 3.7, lp = normal_lpdf(77 | mu, sd))
  known_code = list(
    "A = 33;",
    "B = 57;",
    "mu = A + B;",
    "sd = 3.7;",
    "lp = normal_lpdf(77 | mu, sd);"
  )
  gen_code = swr::stringify_statements(o)
  testthat::expect_equal(known_code, gen_code)
})

test_that("we can check if an object is a statement, and the reverse", {
  z = swr:::BareStatementType$new()
  testthat::expect_true(swr:::is_statement(z))
  testthat::expect_false(swr:::is_not_statement(z))
  testthat::expect_false(swr:::is_statement(33))
  testthat::expect_true(swr:::is_not_statement(5.5))
})

test_that("we can create a single statement type", {
  e = rlang::env()
  e$a = swr:::int("a", value = 33)
  e$b = swr:::real("b", value = 5.5)
  o = swr:::SingleStatementType$new(a + b, declarations = e)
  decl_known = list(
    a = "int a = 33;",
    b = "real b = 5.5;"
  )
  testthat::expect_equal(o$textify_declarations(), decl_known)
  testthat::expect_equal(o$textify_statements(), list("a + b;"))
  testthat::expect_true(swr:::is_statement(o))
})

test_that("we can create a multi-statement type", {
  e = rlang::env()
  e$a = swr:::int("a", value = 33)
  e$b = swr:::real("b", value = 5.5)
  e$d = swr:::real("d", value = 77.2)
  e$c = swr:::real("c", value = 2.5)
  Q = swr:::MultiStatementType$new(a * b, d / c, declarations = e)
  o = swr:::MultiStatementType$new(a + b, Q, declarations = e)
  decl_known = list(
    "int a = 33;",
    "real b = 5.5;",
    "real d = 77.2;",
    "real c = 2.5;"
  )
  testthat::expect_equal(o$textify_declarations(), decl_known)
  testthat::expect_equal(o$textify_statements(), list("a + b;", "a * b;", "d/c;"))
  testthat::expect_true(swr:::is_statement(o))
})




