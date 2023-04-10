
box_func <- function(x){
  iqr <- IQR(x)
  q3 <- quantile(x, probs = c(0.25, 0.5, 0.75), names = FALSE)
  c("min" = q3[1] - 1.5*iqr, "q1" = q3[1], "med" = q3[2],
    "q3" = q3[3], "max" = q3[3] + 1.5*iqr)
}
# Prepare data
val <- split(ToothGrowth$len, list(ToothGrowth$supp, ToothGrowth$dose))
val <- lapply(val, box_func)

dat <- do.call(rbind, val)
dat <- data.frame(Dose = row.names(dat),
                  dat, row.names = NULL)


test_that("Box plot single", {
  dat$Box <- paste(rep(" ", 20), collapse = " ")

  # Draw single group box plot
  tm <- forest_theme(ci_Theight = 0.2)

  p <- forest(dat[,c(1, 7)],
              est = dat$med,
              lower = dat$min,
              upper = dat$max,
              # sizes = sizes,
              fn_ci = make_boxplot,
              ci_column = 2,
              lowhinge = dat$q1,
              uphinge = dat$q3,
              hinge_height = 0.2,
              index_args = c("lowhinge", "uphinge"),
              gp_box = gpar(fill = "black", alpha = 0.4),
              theme = tm
  )

  vdiffr::expect_doppelganger("boxplot-single", p)

})



test_that("Box plot with groups", {
  # Prepare data
  dat_oj <- dat[c(1, 3, 5),]
  dat_vc <- dat[c(2, 4, 6), ]

  dat <- data.frame(Dpse = c(0.5, 1, 2))
  dat$Box <- paste(rep(" ", 20), collapse = " ")

  # Draw plot
  tm <- forest_theme(ci_Theight = 0.2,
                     ci_pch = 3)

  p <- forest(dat,
              est = list(dat_oj$med, dat_vc$med),
              lower = list(dat_oj$min, dat_vc$min),
              upper = list(dat_oj$max, dat_vc$max),
              fn_ci = make_boxplot,
              ci_column = 2,
              lowhinge = list(dat_oj$q1, dat_vc$q1),
              uphinge = list(dat_oj$q3, dat_vc$q3),
              hinge_height = 0.2,
              index_args = c("lowhinge", "uphinge"),
              theme = tm
  )

  vdiffr::expect_doppelganger("boxplot-groups", p)

})


test_that("Inside xlim box plot single", {

  df <- structure(list(Var = c("A", "B", "C", "D", "E", "F", "G", "H"),
                       est1 = c(50, 30, 80, 80, 80, 80, 80, NA),
                       est2 = c(50, 20, 80, 80, 80, 80, 80, 75),
                       up1 = c(70, 40, 250, 150, 100, 100, 310, NA),
                       up2 = c(55, 25, 85, 85, 85, 85, 85, 80),
                       lw1 = c(30, 20, 10, 5, 50, 70, 5, NA),
                       lw2 = c(45, 15, 75, 75, 75, 75, 75, 70),
                       lhg1 = c(0, 0, 30, 30, 50, 70, 0, 0),
                       lhg2 = rep(NA, 8),
                       uhg1 = c(0, 0, 110, 100, 100, 100, 0, 0),
                       uhg2 = rep(NA, 8)),
                  class = "data.frame", row.names = c(NA, -8L))
  df$Box <- paste(rep(" ", 20), collapse = " ")
  df$blk <- paste(rep(" ", 5), collapse = " ")
  df$Box2 <- paste(rep(" ", 20), collapse = " ")

  tm <- forest_theme(ci_Theight = 0.2)

  p <- forest(df[ , c(1, 12:14)],
              est = list(df$est1, df$est2),
              lower = list(df$lw1, df$lw2),
              upper = list(df$up1, df$up2),
              sizes = 0.35,
              fn_ci = make_boxplot,
              ci_column = c(2, 4),
              lowhinge = list(df$lhg1, df$lhg2),
              uphinge = list(df$uhg1, df$uhg2),
              xlim = list(c(10, 200), c(30, 85)),
              x_trans = c("none", "log2"),
              hinge_height = 0.2,
              is_summary=c(rep(F, nrow(df)-1), T),
              index_args= c("lowhinge", "uphinge"),
              theme=tm)

  vdiffr::expect_doppelganger("xlim-boxplot-single", p)

})


