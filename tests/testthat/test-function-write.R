test_that("multiplication works", {
  o = rlang::quo(a + b * -c + f(g))
  s = swr::string_order(swr::stringify(o))
  testthat::expect_equal(s, "a + b * -c + f(g)")
})

test_that("dimension generation", {
  D1 = swr::Dimensions$new()
  testthat::expect_null(D1$textify())
  D2 = swr::Dimensions$new(3, 6, X, Z * 2)
  testthat::expect_equal(D2$textify(), "[3, 6, X, Z * 2]")
  D3 = swr::Dimensions$new(0)
  testthat::expect_equal(D3$textify(), "[0]")
})

test_that("generate null transforms works", {
  testthat::expect_null(swr::transforms()$textify())
})

test_that("generate full transforms works", {
  s = swr::Transforms$new(
    lb = swr::LowerBound$new(33),
    ub = swr::UpperBound$new(Z + 33),
    offset = swr::Offset$new(12),
    scale = swr::Scale$new(0.03)
  )$textify()
  check = "<lower=33, upper=Z + 33, offset=12, scale=0.03>"
  testthat::expect_equal(s, check)
})

test_that("generate partial transforms works", {
  s = swr::Transforms$new(
    ub = swr::UpperBound$new(Z + 33),
    offset = swr::Offset$new(12),
    scale = swr::Scale$new(0.03)
  )$textify()
  check = "<upper=Z + 33, offset=12, scale=0.03>"
  testthat::expect_equal(s, check)
})

test_that("generating types directly works", {

  X = swr::RealType$new("X", 
    swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(1)), 
    value = Z * 7
  )
  testthat::expect_equal(X$textify(), "real<lower=0, upper=1> X = Z * 7;")
  K = swr::IntType$new("K", 
    swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), 
    value = Z * 8
  )
  testthat::expect_equal(K$textify(), "int<lower=0, upper=2> K = Z * 8;")
  K1 = swr::IntType$new("K1", 
    swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), 
    swr::Dimensions$new(3,6, X, A), 
    value = Z * 8
  )
  testthat::expect_equal(K1$textify(), "int<lower=0, upper=2> K1[3, 6, X, A] = Z * 8;")
  K2 = swr::RealType$new("K2", 
    swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), 
    swr::Dimensions$new(3,6, X, A), 
    value = Z * 8
  )
  testthat::expect_equal(K2$textify(), "real<lower=0, upper=2> K2[3, 6, X, A] = Z * 8;")
  X2 = swr::VectorType$new("X2", 
    swr::Transforms$new(), 
    size = swr::dimensions(3 * A), 
    value = Z / 3)
  testthat::expect_equal(X2$textify(), "vector[3 * A] X2 = Z / 3;")
})

test_that("api works as intended for integers", {
  Z1 = swr::int("Z1", value = K + 1)
  testthat::expect_equal(Z1$textify(), "int Z1 = K + 1;")
  Z2 = swr::int("Z2") %>% swr::set(x * 10) %>% swr::stringify()
  testthat::expect_equal(Z2, "int Z2 = x * 10;")
  Z3 = swr::int("Z3") %>% swr::stringify()
  testthat::expect_equal(Z3, "int Z3;")
  Z4 = swr::int("Z4", dimensions = swr::dimensions(3, 6, X, Z * 2))
  testthat::expect_equal(Z4$textify(), "int Z4[3, 6, X, Z * 2];")
})

test_that("api works as intended for real numbers", {
  x1 = swr::real("x1", value = K + 3)
  testthat::expect_equal(x1$textify(), "real x1 = K + 3;")
  x2 = swr::real("x2")
  testthat::expect_equal(x2$textify(), "real x2;")
  x3 = swr::real("x3", dimensions = swr::dimensions(3, 6, X, Z * 2), value = X + 3)
  testthat::expect_equal(x3$textify(), "real x3[3, 6, X, Z * 2] = X + 3;")
})

test_that("api works as intended for vectors", {
  swr::vector("Q", size = swr::dimensions(3 * Z / 4))$textify()
  swr::vector("Q", swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), 
                        size = swr::dimensions(3 * Z / 4), value = erm / 4)$textify()
  testthat::expect_error(swr::vector("Q", size = swr::dimensions(3 * Z / 4, 4))$textify())
})

test_that("api works as intended for row_vectors", {
  swr::row_vector("Q", size = swr::dimensions(3 * Z / 4))$textify()
  swr::row_vector("Q", swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), 
                        size = swr::dimensions(3 * Z / 4), value = erm / 4)$textify()
  testthat::expect_error(
    swr::row_vector("Q", size = swr::dimensions(3 * Z / 4, 4)), class = "simpleError")
})  
