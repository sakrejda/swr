test_that("multiplication works", {
  expect_equal(2 * 2, 4)
  o = rlang::quo(a + b * !c + f(g))
  string_order(stringify(o))
  str(stringify(o))
})
swr::Transforms$new(
  lb = swr::LowerBound$new(33),
  ub = swr::UpperBound$new(Z + 33),
  offset = swr::Offset$new(12),
  swr::Scale = scale$new(0.03)
)$textify()
swr::RealType$new("X", swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(1)), Z * 7)$textify()
swr::IntType$new("K", swr::Transforms$new(swr::LowerBound$new(0), swr::UpperBound$new(2)), Z * 8)$textify()

swr::transforms()$textify()
swr::int("Z", value = K + 1)$textify()
swr::real("Z", value = K + 3)$textify()


swr::Dimensions$new()$textify()
swr::Dimensions$new(3, 6, X, Z * 2)$textify()
