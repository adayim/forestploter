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
                   is_exp = TRUE)
  expect_equal(xlm, c(log(min(dt$low, na.rm = TRUE)),
                      log(max(dt$hi, na.rm = TRUE))))

})





