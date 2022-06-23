# Test themes

test_that("Test themes default value", {
  tm <- forest_theme()
  expect_type(tm, "list")
  expect_equal(names(tm), c('legend', 'ci', 'xaxis', 'footnote', "title",
                            'refline', 'vertline', 'summary', 'tab_theme'))

  expect_identical(tm$legend, list('fontsize' = 12, 'fontfamily' = "",
                                   'name' = "Group", 'position' = "right",
                                   'label' = ""))

  expect_identical(tm$ci, list('pch' = 15, 'col' = "black", 'lty' = 1,
                               'lwd' = 1, 't_height' = NULL))

})

test_that("Set theme", {
  tm <- forest_theme(legend_value = c("Gp1", "Gp2"))
  expect_type(tm, "list")

  expect_identical(tm$legend, list('fontsize' = 12, 'fontfamily' = "",
                                   'name' = "Group", 'position' = "right",
                                   'label' = c("Gp1", "Gp2")))

  expect_identical(tm$ci, list('pch' = c(15, 15), 'col' = c("#e41a1c","#377eb8"),
                               'lty' = c(1, 1), 'lwd' = c(1, 1),
                               't_height' = NULL))

})
