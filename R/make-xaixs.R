
make_xais <- function(at = NULL, label_at = NULL, labels = NULL, gp = gpar(), xlim){

  if(is.null(at))
    at <- pretty(xlim)

  if(is.null(labels))
    labels <- as.character(at)

  maj <- linesGrob(x = unit(c(min(xlim), max(xlim)), "native"),
                   y = unit(c(0.99, 0.99), "npc"),
                   gp = gp,
                   name="major")

  maj_cord <- getCorners(maj)

  tick <- segmentsGrob(x0 = unit(at, "native"), y0 = maj_cord$yb,
                       x1 = unit(at, "native"), y1 = maj_cord$yb - unit(.5, "lines"),
                       gp = gp,
                       name = "tick")

  lab <- textGrob(labels, x = unit(label_at, "native"), y = maj_cord$yb - unit(1, "lines"),
                  gp = gp,
                  just="centre", rot=0, check.overlap=TRUE,
                  name = "label")

  grobTree(gList(maj, tick, lab),
           vp = viewport(xscale = xlim),
           name = "xaxis")
}
