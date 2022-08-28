
test_that("test scales", {

  # Trans
  expect_equal(xscale(3.5, "log"), log(3.5))
  expect_equal(xscale(3.5, "log2"), log2(3.5))
  expect_equal(xscale(3.5, "log10"), log10(3.5))

  # Inv
  expect_equal(xscale(3.5, "log", "inv"), exp(3.5))
  expect_equal(xscale(3.5, "log2", "inv"), 2^3.5)
  expect_equal(xscale(3.5, "log10", "inv"), 10^3.5)
  expect_equal(xscale(log(3.5), "log", "inv"), 3.5)
  expect_equal(xscale(log2(3.5), "log2", "inv"), 3.5)
  expect_equal(xscale(log10(3.5), "log10", "inv"), 3.5)

  # Math expression
  expect_equal(math_exp(3.5, 2^x), expression(2^3.5))
  expect_equal(math_exp(3.5, 10^x), expression(10^3.5))

  # Scientific notation
  expect_equal(xscale(pi, "scientific", "format"), "3.1e+00")
  expect_equal(xscale(pi, "scientific", "format", 1L), "3.1")
})
