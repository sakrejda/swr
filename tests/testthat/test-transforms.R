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

