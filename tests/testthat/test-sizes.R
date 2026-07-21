
#### Prep data
dt <- read.csv(system.file("extdata", "example_data.csv", package = "forestploter"))
dt <- dt[1:6, 1:6]
dt$Treatment <- ifelse(is.na(dt$Treatment), "", dt$Treatment)
dt$Placebo <- ifelse(is.na(dt$Placebo), "", dt$Placebo)
dt$` ` <- paste(rep(" ", 20), collapse = " ")

meta_w <- read.csv(system.file("extdata", "metadata.csv", package = "forestploter"))$weights
meta_w <- meta_w[!is.na(meta_w) & meta_w < 100]

mkplot <- function(...){
  forest(dt[, c(1:3, 7)],
         est = dt$est,
         lower = dt$low,
         upper = dt$hi,
         ci_column = 4,
         ...)
}


test_that("scale_sizes maps weights onto the requested range", {

  # `range` pins both ends, as metafor's `plim` does
  rng <- scale_sizes(meta_w, range = c(0.2, 0.8), method = "range")
  expect_equal(min(rng), 0.2)
  expect_equal(max(rng), 0.8)

  # Order is preserved and the square root is taken before scaling, so the
  # areas rather than the diameters follow the weights
  expect_equal(order(rng), order(meta_w))
  expect_equal(rng, (sqrt(meta_w) - min(sqrt(meta_w))) /
                 (max(sqrt(meta_w)) - min(sqrt(meta_w))) * 0.6 + 0.2)

  # `proportional` keeps the largest on range[2] and clamps the smallest up,
  # as meta does
  prop <- scale_sizes(meta_w, range = c(0.2, 0.8), method = "proportional")
  expect_equal(max(prop), 0.8)
  expect_true(min(prop) >= 0.35 * 0.8)
  expect_equal(prop, pmax(sqrt(meta_w) / max(sqrt(meta_w)), 0.35) * 0.8)

  # The floor is what stops small studies vanishing
  expect_equal(min(scale_sizes(c(1, 10000), method = "proportional")), 0.35 * 0.8)
})

test_that("scale_sizes handles NA and degenerate weights", {

  # NA in, NA out, so non-study rows pass through untouched
  out <- scale_sizes(c(meta_w, NA))
  expect_true(is.na(out[length(out)]))
  expect_equal(out[-length(out)], scale_sizes(meta_w))

  # All weights equal leaves nothing to scale against
  expect_equal(unique(scale_sizes(rep(5, 4))), 0.5)

  expect_equal(scale_sizes(c(NA_real_, NA_real_)), c(NA_real_, NA_real_))

  expect_error(scale_sizes(meta_w, range = c(0.8, 0.2)),
               "must be in increasing order")
  expect_error(scale_sizes(meta_w, range = c(0, 0.8)),
               "must be larger than 0")
  expect_error(scale_sizes(meta_w, range = 0.5),
               "numeric vector of length 2")
})

test_that("size_method scales weights inside forest", {

  p <- mkplot(sizes = dt$est * 10, size_method = "range", size_range = c(0.2, 0.8))
  expect_s3_class(p, "forestplot")

  # `none` is the default and leaves the values alone
  expect_identical(formals(forest)$size_method[[2]], "none")
})

test_that("summary rows are held out of the weight scaling", {

  # A pooled total is not a study weight. Left in the normalisation it would
  # take the top of the range and squash every study towards the bottom, so it
  # is excluded and drawn at size_range[2] instead - the same rule `meta` gets
  # from giving its pooled rows no study weight.
  w <- c(10.2, 18.4, 19.9, 5.4, 21.6, 100)
  is_sum <- c(rep(FALSE, 5), TRUE)

  p <- forest(dt[, c(1:3, 7)], est = dt$est, lower = dt$low, upper = dt$hi,
              ci_column = 4, sizes = w, size_method = "range",
              size_range = c(0.2, 0.8), is_summary = is_sum)

  # Pull the size actually handed to each CI grob back out of the gtable. Study
  # rows are `makeci` gTrees carrying `size`; the summary is a bare polygon, so
  # its height has to be measured instead.
  ci <- p$grobs[grepl("^ci-", p$layout$name)]
  is_poly <- vapply(ci, function(g) inherits(g, "polygon"), logical(1))

  studies <- vapply(ci[!is_poly], function(g) g$size, numeric(1))

  # Only rows carrying an estimate get a grob, so compare against the scaled
  # weights of exactly those rows
  expected <- scale_sizes(replace(w, 6, NA), range = c(0.2, 0.8))
  expect_equal(min(expected, na.rm = TRUE), 0.2)
  expect_equal(max(expected, na.rm = TRUE), 0.8)
  expect_equal(unname(studies), unname(expected[!is.na(dt$est) & !is_sum]))

  # The diamond is drawn at the top of the range, matching the largest study
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  dia <- ci[is_poly][[1]]
  height <- convertHeight(dia$y[2], "bigpts", TRUE) -
    convertHeight(dia$y[4], "bigpts", TRUE)
  expect_equal(height, 0.8 * 12)

  # The studies span the whole range only because the pooled row was excluded.
  # Left in, its weight of 100 would crush every study into the bottom third.
  crushed <- scale_sizes(w, range = c(0.2, 0.8))
  expect_lt(max(crushed[!is_sum]), 0.5)
})

test_that("forest warns on sizes outside the usable range", {

  expect_warning(mkplot(sizes = 0.01), "outside the usual range")
  expect_warning(mkplot(sizes = 5), "outside the usual range")

  # The band covers every mainstream weight-scaling convention, so the values
  # used across the docs and tests must not trip it
  expect_no_warning(mkplot(sizes = 0.4))
  expect_no_warning(mkplot(sizes = sqrt(meta_w[1:6] / 100)))
  expect_no_warning(mkplot(sizes = (log(dt$hi) - log(dt$est)) / 1.96))

  # Raw weights are checked after scaling, not before, so passing weights that
  # would trip the bar untransformed is silent when `size_method` is set
  expect_no_warning(mkplot(sizes = meta_w[1:6], size_method = "range"))
  expect_warning(mkplot(sizes = meta_w[1:6]), "outside the usual range")
})

test_that("forest warns when grouped CIs will overlap", {

  grouped <- function(...)
    forest(dt[, c(1:3, 7)],
           est = list(dt$est, dt$est * 1.1),
           lower = list(dt$low, dt$low * 1.1),
           upper = list(dt$hi, dt$hi * 1.1),
           ci_column = 4,
           ...)

  expect_warning(grouped(sizes = 1.5, nudge_y = 0.1), "more than twice the gap")

  # Enough room between the groups, so no complaint
  expect_no_warning(grouped(sizes = 0.4, nudge_y = 0.6))

  # Points that touch slightly are legible and must stay silent, otherwise the
  # package's own grouped examples would warn
  expect_no_warning(grouped(sizes = 0.4, nudge_y = 0.2))
})

test_that("summary rows grow to fit stacked diamonds", {

  is_sum <- c(rep(FALSE, 5), TRUE)

  grouped <- function(sizes)
    forest(dt[, c(1:3, 7)],
           est = list(dt$est, dt$est * 1.1),
           lower = list(dt$low, dt$low * 1.1),
           upper = list(dt$hi, dt$hi * 1.1),
           sizes = sizes, ci_column = 4, nudge_y = 0.6,
           is_summary = is_sum)

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  # Diamonds at +/-0.3 npc need a row of at least (s*base_size/2)/(0.5-0.3)
  # bigpts. At 0.4 that is 12bp, less than the natural row, so nothing grows.
  small <- convertHeight(grouped(0.4)$heights, "bigpts", valueOnly = TRUE)
  expect_equal(sum(small > 1e-6 & small == max(small)), 1L)

  # At 0.7 it is 21bp, which exceeds the natural row height, so the summary row
  # and only the summary row grows
  big <- convertHeight(grouped(0.7)$heights, "bigpts", valueOnly = TRUE)
  expect_equal(sum(big - small > 1e-6), 1L)
  expect_equal(max(big - small) + small[which.max(big - small)], 21)
})

test_that("point size follows base_size rather than the device", {

  # A `char` unit would have resolved against the device pointsize, leaving the
  # point identical at every base_size
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)

  psize <- function(bs){
    p <- mkplot(sizes = 0.4, theme = forest_theme(base_size = bs))
    ci <- p$grobs[[which(grepl("^ci-", p$layout$name))[1]]]
    convertHeight(makeContent(ci)$children$point$size, "bigpts", valueOnly = TRUE)
  }

  expect_equal(psize(12), 0.4 * 12)
  expect_equal(psize(24), 0.4 * 24)
  expect_equal(psize(8), 0.4 * 8)
})
