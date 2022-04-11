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
  testthat::expect_equal(X2$textify(), "vector[3 * A] X2 = Z/3;")
  X3 = swr::RowVectorType$new("X3", 
    swr::Transforms$new(), 
    size = swr::dimensions(3 * A), 
    value = Z / 3)
  testthat::expect_equal(X3$textify(), "row_vector[3 * A] X3 = Z/3;")
  X4 = swr::SimplexType$new("X4", 
    swr::Transforms$new(), 
    size = swr::dimensions(3 * A), 
    value = Z / 3)
  testthat::expect_equal(X4$textify(), "simplex[3 * A] X4 = Z/3;")
  A = swr::MatrixType$new("A",
    size = swr::dimensions(33,2 / Z),
    value = B * 2)
  testthat::expect_equal(A$textify(), "matrix[33, 2/Z] A = B * 2;")
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

test_that("api works as intended for simplexes", {
  swr::simplex("Q", size = swr::dimensions(3 * Z / 4))$textify()
  swr::simplex("Q", swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), 
                        size = swr::dimensions(3 * Z / 4), value = erm / 4)$textify()
  testthat::expect_error(swr::simplex("Q", size = swr::dimensions(3 * Z / 4, 4))$textify())
})

test_that("api works as intended for unit_vectors", {
  swr::unit_vector("Q", size = swr::dimensions(3 * Z / 4))$textify()
  swr::unit_vector("Q", swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), 
                        size = swr::dimensions(3 * Z / 4), value = erm / 4)$textify()
  testthat::expect_error(swr::unit_vector("Q", size = swr::dimensions(3 * Z / 4, 4))$textify())
})

test_that("api works as intended for ordereds", {
  swr::ordered("Q", size = swr::dimensions(3 * Z / 4))$textify()
  swr::ordered("Q", swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), 
                        size = swr::dimensions(3 * Z / 4), value = erm / 4)$textify()
  testthat::expect_error(swr::ordered("Q", size = swr::dimensions(3 * Z / 4, 4))$textify())
})

test_that("api works as intended for positive_ordereds", {
  swr::positive_ordered("Q", size = swr::dimensions(3 * Z / 4))$textify()
  swr::positive_ordered("Q", swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), 
                        size = swr::dimensions(3 * Z / 4), value = erm / 4)$textify()
  testthat::expect_error(swr::positive_ordered("Q", size = swr::dimensions(3 * Z / 4, 4))$textify())
})


test_that("matrix's api works as intended", {
  swr::matrix("Q", size = swr::dimensions(3 * Z / 4, 100))$textify()
  swr::matrix("Q", swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), 
                        size = swr::dimensions(3 * Z / 4, 2), value = erm / 4)$textify()
  testthat::expect_error(swr::matrix("Q", size = swr::dimensions(3 * Z / 4))$textify())
})

test_that("cov_matrix's api works as intended", {
  swr::cov_matrix("Q", size = swr::dimensions(3 * Z / 4))$textify()
  swr::cov_matrix("Q", swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), 
                        size = swr::dimensions(3 * Z / 4), value = erm / 4)$textify()
  testthat::expect_error(swr::cov_matrix("Q", size = swr::dimensions(3 * Z / 4,2))$textify())
})

test_that("corr_matrix's api works as intended", {
  swr::corr_matrix("Q", size = swr::dimensions(3 * Z / 4))$textify()
  swr::corr_matrix("Q", swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), 
                        size = swr::dimensions(2), value = erm / 4)$textify()
  testthat::expect_error(swr::corr_matrix("Q", size = swr::dimensions(3 * Z / 4,3))$textify())
})

test_that("cholesky_factor_cov_matrix's api works as intended", {
  swr::cholesky_factor_cov("Q", size = swr::dimensions(3 * Z / 4, 100))$textify()
  swr::cholesky_factor_cov("Q", swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), 
                        size = swr::dimensions(3 * Z / 4, 2), value = erm / 4)$textify()
  testthat::expect_error(swr::cholesky_factor_cov("Q", size = swr::dimensions(3 * Z / 4, 3, 5))$textify())
})
