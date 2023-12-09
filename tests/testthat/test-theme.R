# Test themes

test_that("Test themes default value", {
  tm <- forest_theme()
  expect_type(tm, "list")
  expect_equal(names(tm), c('legend', 'ci', 'xaxis', 'footnote', "title",
                            'arrow', 'refline', 'vertline', 'xlab',
                            'summary', 'tab_theme'))

  expect_identical(tm$legend, list(gp = gpar(fontsize = 12, fontfamily = "", cex=1),
                                   'name' = "Group", 'position' = "right",
                                   'label' = ""))

  expect_identical(tm$ci, list('pch' = 15, 'col' = "black",
                               'fill' = NULL, 'lty' = 1, 'alpha' = 1,
                               'lwd' = 1, 't_height' = NULL))

})

test_that("Set theme", {
  tm <- forest_theme(legend_value = c("Gp1", "Gp2"))
  expect_type(tm, "list")

  expect_identical(tm$legend, list(gp = gpar('fontsize' = 12, 'fontfamily' = "", "cex"=1),
                                   'name' = "Group", 'position' = "right",
                                   'label' = c("Gp1", "Gp2")))

  expect_identical(tm$ci, list('pch' = c(15, 15), 'col' = c("#e41a1c","#377eb8"),
                               'fill' = c("#e41a1c","#377eb8"),
                               'lty' = c(1, 1), 'alpha' = c(1, 1),
                               'lwd' = c(1, 1), 't_height' = NULL))

  tm <- forest_theme(legend_value = c("Gp1", "Gp2"), ci_fill = "black")
  expect_identical(tm$ci$col, tm$ci$fill)
  expect_identical(tm$ci$col, c("black", "black"))

  tm <- make_group_theme(tm, 2)
  expect_identical(tm$ci$col, c("#e41a1c", "#377eb8"))

})

test_that("Test errors", {
  expect_error(forest_theme(ci_fill = c("#e41a1c","#377eb8")),
                 "legend_value should be provided each groups.")

  expect_warning(forest_theme(ci_fill = "#e41a1c", ci_pch = 1),
                 "`ci_pch` is not within 15:25, `ci_fill` will be ignored.")

  expect_error(forest_theme(ci_alpha = c(0.4, 0.3)),
                 "`ci_alpha` must be of length 1.")

  expect_error(forest_theme(ci_fill = c("#e41a1c","#377eb8"),
                            legend_value = letters[1:3],
                            ci_col = c("#e41a1c","#377eb8", "black")),
               "`ci_fill` must be of length 1 or same length as `ci_col`.")


})

