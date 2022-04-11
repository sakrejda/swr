
test_that("dimension generation", {
  D1 = swr::Dimensions$new()
  testthat::expect_null(D1$textify())
  D2 = swr::Dimensions$new(3, 6, X, Z * 2)
  testthat::expect_equal(D2$textify(), "[3, 6, X, Z * 2]")
  D3 = swr::Dimensions$new(0)
  testthat::expect_equal(D3$textify(), "[0]")
})

