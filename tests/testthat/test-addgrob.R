
#### Prep data
dt <- read.csv(system.file("extdata", "metadata.csv",
                           package = "forestploter"))

dt$cicol <- paste(rep(" ", 20), collapse = " ")

# Select some columns for plotting, this will be used as a skeleton of the forestplot
dt_fig <- dt[,c(1:7, 17, 8:13)]

colnames(dt_fig) <- c("Study or Subgroup",
                      "Events","Total","Events","Total",
                      "Weight",
                      "", "",
                      LETTERS[1:6])

dt_fig$Weight <- sprintf("%0.1f%%", dt_fig$Weight)
dt_fig$Weight[dt_fig$Weight == "NA%"] <- ""

# Convert NA to blank string
dt_fig[is.na(dt_fig)] <- ""


test_that("Add grob", {

  tm <- forest_theme(core = list(bg_params=list(fill = c("white"))),
                     summary_col = "black",
                     arrow_label_just = "end",
                     arrow_type = "closed")

  p <- forest(dt_fig,
              est = dt$est,
              lower = dt$lb,
              upper = dt$ub,
              sizes = sqrt(dt$weights/100),
              is_summary = c(rep(F, nrow(dt)-1), T),
              ci_column = 8,
              ref_line = 1,
              x_trans = "log",
              arrow_lab = c("Favours caffeine","Favours decaf"),
              xlim = c(0.05, 100),
              ticks_at = c(0.1, 1, 10, 100),
              theme = tm)

  g <- add_grob(p,
                row = 1:c(nrow(dt_fig) - 1),
                col = 9:14,
                order = "backgroud",
                gb_fn = roundrectGrob,
                r = unit(0.05, "snpc"),
                gp = gpar(lty = "dotted",
                          col = "#bdbdbd"))

  vdiffr::expect_doppelganger("add-grob-body", g)

  g <- add_grob(p,
                row = 1,
                col = 9:14,
                order = "top",
                part = "header",
                gb_fn = roundrectGrob,
                r = unit(0.05, "snpc"),
                gp = gpar(lty = "dotted",
                          col = "#bdbdbd"))

  g <- add_text(g, text = "Pm[2.5]",
                part = "header",
                col = 7:8,
                gp = gpar(fontface = "bold"),
                parse = TRUE)

  g <- insert_text(g,
                   text = c("S^2", "A", "12", "B", "C"),
                   part = "header",
                   col = 2:6,
                   parse = TRUE)

  vdiffr::expect_doppelganger("add-grob-header", g)

  wh <- get_wh(g)
  expect_equal(unname(wh), c(10.43, 3.64),
               tolerance = 0.01)

})

