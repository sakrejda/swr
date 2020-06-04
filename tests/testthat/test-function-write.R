

test_that("stringify_declarations works as intended", {
  Q = swr::matrix("Q", swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), 
                        size = swr::dimensions(3 * Z / 4, 2), value = erm / 4)
  x1 = swr::real("x1", value = K + 3)
  e_test = rlang::env()
  e_test$Q = Q
  e_test$x1 = x1
  code = rlang::quos(dd = Q * 2 / x1)
  text = swr:::stringify_declaration(code, e_test)
  testthat::expect_equal(text[['Q']], "matrix<lower=0, upper=2>[3 * Z/4, 2] Q = erm/4;")
  testthat::expect_equal(text[['x1']], "real x1 = K + 3;")
})


