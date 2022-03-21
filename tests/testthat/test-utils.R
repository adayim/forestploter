# make_xlim

dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))

test_that("Check make_xlim", {
  # Check simple
  xlm <- make_xlim(lower = list(dt$low),
                   upper = list(dt$hi),
                   gp_num = 1,
                   ref_line = 1)
  expect_equal(length(xlm), 1)
  expect_equal(length(xlm[[1]]), 2)

  # Check multiple 1 group and 1 columns
  xlm <- make_xlim(lower = list(dt$low,
                                dt$low_gp1,
                                dt$low_gp2),
                   upper = list(dt$hi,
                                dt$hi_gp1,
                                dt$hi_gp2),
                   gp_num = 3,
                   ref_line = 1)
  expect_equal(length(xlm), 1)
  ln <- sapply(xlm, length)
  expect_equal(unique(ln), 2)

  # Check multiple 1 group and multiple columns
  xlm <- make_xlim(lower = list(dt$low,
                                dt$low_gp1,
                                dt$low_gp2),
                   upper = list(dt$hi,
                                dt$hi_gp1,
                                dt$hi_gp2),
                   gp_num = 1,
                   ref_line = 1)
  expect_equal(length(xlm), 3)
  ln <- sapply(xlm, length)
  expect_equal(unique(ln), 2)

  # Check multiple 2 group and 1 columns
  xlm <- make_xlim(lower = list(dt$low_gp1,
                                dt$low_gp2),
                   upper = list(dt$hi_gp1,
                                dt$hi_gp2),
                   gp_num = 2,
                   ref_line = 1)
  expect_equal(length(xlm), 1)
  expect_equal(length(xlm[[1]]), 2)

  # Check multiple 2 group and 2 columns
  xlm <- make_xlim(lower = list(dt$low,
                                dt$low_gp1,
                                dt$low_gp2,
                                dt$low_gp3),
                   upper = list(dt$hi,
                                dt$hi_gp1,
                                dt$hi_gp2,
                                dt$low_gp3),
                   gp_num = 2,
                   ref_line = 1)
  expect_equal(length(xlm), 2)
  ln <- sapply(xlm, length)
  expect_equal(unique(ln), 2)

  # Check multiple 2 group and 2 columns given xlim
  xlm <- make_xlim(lower = list(dt$low,
                                dt$low_gp1,
                                dt$low_gp2,
                                dt$low_gp3),
                   upper = list(dt$hi,
                                dt$hi_gp1,
                                dt$hi_gp2,
                                dt$low_gp3),
                   xlim = c(-1, 2),
                   gp_num = 2,
                   ref_line = 1)
  expect_equal(length(xlm), 2)
  ln <- sapply(xlm, length)
  expect_equal(unique(ln), 2)


})



test_that("Check make_xlim value", {

  # Check xlim value 4 group 1 column
  xlm <- make_xlim(lower = list(dt$low_gp1,
                                dt$low_gp2,
                                dt$low_gp3,
                                dt$low_gp4),
                   upper = list(dt$hi_gp1,
                                dt$hi_gp2,
                                dt$hi_gp3,
                                dt$hi_gp4),
                   gp_num = 4,
                   ref_line = 1)

  expc_val <- c(min(dt[,paste0("low_gp", 1:4)], na.rm = TRUE),
                max(dt[,paste0("hi_gp", 1:4)], na.rm = TRUE))

  expect_equal(xlm[[1]], expc_val)

  # Check xlim value 1 group 4 column
  xlm <- make_xlim(lower = list(dt$low_gp1,
                                dt$low_gp2,
                                dt$low_gp3,
                                dt$low_gp4),
                   upper = list(dt$hi_gp1,
                                dt$hi_gp2,
                                dt$hi_gp3,
                                dt$hi_gp4),
                   gp_num = 1,
                   ref_line = 1)

  expc_low <- lapply(dt[,paste0("low_gp", 1:4)], min, na.rm = TRUE)
  expc_hi <- lapply(dt[,paste0("hi_gp", 1:4)], max, na.rm = TRUE)
  expc_val <- mapply(c, expc_low, expc_hi, USE.NAMES = FALSE)
  xlm <- do.call(cbind, xlm)
  expect_equal(xlm, expc_val)

  # Check xlim value 2 group 2 column
  xlm <- make_xlim(lower = list(dt$low_gp1,
                                dt$low_gp2,
                                dt$low_gp3,
                                dt$low_gp4),
                   upper = list(dt$hi_gp1,
                                dt$hi_gp2,
                                dt$hi_gp3,
                                dt$hi_gp4),
                   gp_num = 2,
                   ref_line = 1)

  expc_val1 <- c(min(dt[,paste0("low_gp", c(1, 3))], na.rm = TRUE),
                 max(dt[,paste0("hi_gp", c(1, 3))], na.rm = TRUE))
  expc_val2 <- c(min(dt[,paste0("low_gp", c(2, 4))], na.rm = TRUE),
                 max(dt[,paste0("hi_gp", c(2, 4))], na.rm = TRUE))

  expect_equal(xlm[[1]], expc_val1)
  expect_equal(xlm[[2]], expc_val2)


})


