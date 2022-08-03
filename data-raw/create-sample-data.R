library(terra)
s = rast(system.file("ex/logo.tif", package="terra"))
s2 = colorize(s, to = "col")
plot(s2)
coltab(s2) = NULL
s2
plot(s2)

dir.create("inst/pic", recursive = TRUE)
png("inst/pic/rainbowr.png", width = 600, height = 500)
plot(s2, col = grDevices::rainbow(10))
dev.off()
