% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cblind.plot.R
\name{cblind.prep}
\alias{cblind.prep}
\title{Prepare Input Image for Color Blind Plot}
\usage{
cblind.prep(im, r = 1, g = 2, b = 3, crop_manual = FALSE, select_class = FALSE)
}
\arguments{
\item{im}{A file path to an image}

\item{r}{Index of the Red channel}

\item{g}{Index of the Green channel}

\item{b}{Index of the Blue channel}

\item{crop_manual}{Do you want to manually crop the input image?}

\item{select_class}{Do you want to select only certain colors in the image for the further processing?}
}
\value{
A terra SpatRaster object
}
\description{
Prepares input image for cblind.plot by: a) cropping the image, b) selecting certain colors, c) performing PCA on the image.
}
\examples{
my_image <- system.file("pic/imager.png", package = "cblindplot")
#suppressWarnings(my_image_terra <- terra::rast(my_image))
#terra::plotRGB(my_image_terra)

new_image <- cblind.prep(my_image)
new_image
}
