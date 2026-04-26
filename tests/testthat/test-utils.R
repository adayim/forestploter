# make_xlim

dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))

test_that("Check make_xlim", {
  # Check simple
  xlm <- make_xlim(lower = list(dt$low),
                   upper = list(dt$hi),
                   ref_line = 1)
  expect_equal(xlm, c(min(dt$low, na.rm = TRUE), max(dt$hi, na.rm = TRUE)))

  xlm <- make_xlim(lower = list(dt$low),
                   upper = list(dt$hi),
                   ref_line = 10)
  expect_equal(xlm, c(min(dt$low, na.rm = TRUE), 10))

  xlm <- make_xlim(lower = list(dt$low),
                   upper = list(dt$hi),
                   ref_line = 0)
  expect_equal(xlm, c(0, max(dt$hi, na.rm = TRUE)))

  # Check multiple 1 group and 1 columns
  xlm <- make_xlim(lower = list(dt$low,
                                dt$low_gp1,
                                dt$low_gp2),
                   upper = list(dt$hi,
                                dt$hi_gp1,
                                dt$hi_gp2),
                   ref_line = 1)
  expect_equal(xlm, c(min(c(dt$low, dt$low_gp1, dt$low_gp2), na.rm = TRUE),
                      max(c(dt$hi, dt$hi_gp1, dt$hi_gp2), na.rm = TRUE)))

  # Check EXP
  xlm <- make_xlim(lower = list(dt$low),
                   upper = list(dt$hi),
                   ref_line = 1,
                   x_trans = "log")
  expect_equal(xlm, c(log(min(dt$low, na.rm = TRUE)),
                      log(max(dt$hi, na.rm = TRUE))))

})


test_that("log_pretty produces base-aware ticks", {
  # Wide range: one tick per decade (sub = 1 only)
  expect_equal(log_pretty(c(0.05, 100), base = 10),
               c(0.1, 1, 10, 100))

  # Mid range (1.5 - 3 decades): sub = 1, 2, 5
  expect_equal(log_pretty(c(0.5, 50), base = 10),
               c(0.5, 1, 2, 5, 10, 20, 50))

  # Narrow range (0.5 - 1.5 decades): finer sub-multiples
  expect_equal(log_pretty(c(15, 80), base = 10),
               c(20, 30, 50, 70))

  # log2 base
  expect_equal(log_pretty(c(30, 85), base = 2),
               c(32, 64, 80))

  # Sub-decade range falls back to pretty()
  expect_equal(log_pretty(c(11, 14), base = 10),
               pretty(c(11, 14)))

  # Non-positive input also falls back rather than erroring
  expect_equal(log_pretty(c(-1, 100), base = 10),
               pretty(c(-1, 100)))
})


test_that("make_ticks: linear axis delegates to pretty()", {
  expect_equal(make_ticks(at = NULL, xlim = c(0, 4), refline = 1, x_trans = "none"),
               pretty(c(0, 4))[pretty(c(0, 4)) >= 0 & pretty(c(0, 4)) <= 4])
})


test_that("make_ticks: log10 yields pretty values in transformed space", {
  # xlim arrives in transformed (log10) space
  out <- make_ticks(at = NULL,
                    xlim = log10(c(0.05, 100)),
                    refline = 1,
                    x_trans = "log10")
  # Original-space equivalents should be the canonical 0.1, 1, 10, 100
  expect_equal(sort(round(10 ^ out, 6)), c(0.1, 1, 10, 100))
})


test_that("make_ticks: log2 with refline anchors the reference value", {
  out <- make_ticks(at = NULL,
                    xlim = log2(c(0.5, 8)),
                    refline = 1,
                    x_trans = "log2")
  expect_true(0 %in% round(out, 6))   # log2(1) == 0 must be present
})


test_that("make_ticks: explicit `at` is just transformed and filtered", {
  expect_equal(make_ticks(at = c(0.1, 1, 10),
                          xlim = log10(c(0.05, 100)),
                          refline = 1,
                          x_trans = "log10"),
               c(-1, 0, 1))
})





