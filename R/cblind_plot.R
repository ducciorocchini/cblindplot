# Code related to the manuscript:
# Scientific maps should reach everyone: a straightforward approach to let colour blind people visualise spatial patterns
# by Duccio Rocchini et al.
# Original coda by: Elisa Thouverai rewritten and readapted by Jakub Nowosad

#' Title
#'
#' @param im
#' @param r
#' @param g
#' @param b
#' @param cvd
#' @param crop_manual
#' @param select_class
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' library(terra)
#' my_image <- rast(system.file("pic/rainbowr.png", package = "cblindplot"))
#' plotRGB(my_image)
#'
#' cblind.plot(my_image)
#' #cblind.plot(my_image, crop_manual = TRUE)
#' #cblind.plot(my_image, select_class = TRUE)
cblind.plot = function(im, r = 1, g = 2, b = 3, cvd = c("protanopia", "deuteranopia", "tritanopia"), crop_manual = FALSE, select_class = FALSE, ...){
  cvd <- cvd[1]
  if(!cvd %in% c("protanopia", "deuteranopia", "tritanopia")) stop("Wrong 'cvd` value. It can be 'protanopia', 'deuteranopia', or 'tritanopia'")

  impl <- cblind.prep(im, r = r, g = g, crop_manual = crop_manual, select_class = select_class)
  impl <- as.data.frame(impl, xy = TRUE)[c(1:3)]
  colnames(impl) <- c("x", "y", "values")
  ggt <- ggplot2::ggplot(impl, ggplot2::aes_string(x = "x", y = "y", fill = "values")) +
    ggplot2::geom_raster() +
    # ggplot2::coord_sf() +
    ggplot2::theme(legend.position = "bottom")
  if(cvd == "deuteranopia") {
    pl <- ggt + ggplot2::scale_fill_viridis_c(na.value = "transparent")
  } else if(cvd == "protanopia") {
    pl <- ggt + ggplot2::scale_fill_viridis_c(na.value = "transparent", option = "E")
  } else if(cvd == "tritanopia") {
    pl <- ggt + ggplot2::scale_fill_viridis_c(na.value = "transparent", option = "A")
  }
  return(pl)
}

#' Title
#'
#' @param im
#' @param r
#' @param g
#' @param b
#' @param cvd
#' @param crop_manual
#' @param select_class
#'
#' @return
#' @export
#'
#' @examples
#' library(terra)
#' my_image <- rast(system.file("pic/rainbowr.png", package = "cblindplot"))
#' plotRGB(my_image)
#'
#' new_image <- cblind.prep(my_image)
#' new_image
cblind.prep <- function(im, r = 1, g = 2, b = 3, crop_manual = FALSE, select_class = FALSE){
  im <- cblind.prep.input(im)

  if (crop_manual) {
    if(terra::nlyr(im) == 1) {
      im <- terra::flip(terra::rast(imagefx::crop.image(as.matrix(im, wide = TRUE), pick = TRUE)$img.crop))
    } else {
      coords <- imagefx::crop.image(as.matrix(im[[1]], wide = TRUE), pick = TRUE)$img.corners
      coords_x <- sort(coords[c(3, 4)]) / ncol(im)
      coords_y <- sort(nrow(im) - coords[c(1, 2)]) / nrow(im)
      coords <- c(coords_x, coords_y)
      im <- terra::crop(im, coords)
    }
  }

  if (select_class){
    imcl <- terra::deepcopy(im)
    vals_im <- terra::values(im)
    terra::values(imcl)[!(rowSums(is.na(vals_im)) > 0), ] <- stats::kmeans(stats::na.omit(vals_im), centers = 2)$cluster
    imcldf <- as.data.frame(imcl, xy = TRUE)
    colnames(imcldf) <- c("x", "y", "values")
    imcldf$values <- as.character(imcldf$values)

    pr <- ggplot2::ggplot(imcldf, ggplot2::aes_string(x = "x", y = "y", fill = "values")) +
      ggplot2::geom_tile() +
      # ggplot2::coord_sf() +
      ggplot2::scale_fill_manual(values = c("#000000", "#CCCCCC"))
    print(pr)

    ch <- "0"
    while(ch != "1" && ch != "2") {
      ch <- readline("Class to keep (1/2): ")
    }
    imcl[imcl == as.numeric(ch)] <- NA
    im <- terra::mask(im, imcl, inverse = TRUE)
  }

  if (terra::nlyr(im) == 1){
    impl <- im
  } else {
    impca <- im[[c(r, b, g)]]
    pca_model <- stats::prcomp(stats::na.omit(as.matrix(impca)))
    impl <- terra::predict(impca, pca_model)
  }

  return(impl)
}

cblind.prep.input = function(im){
  if (!inherits(im, "SpatRaster") && !inherits(im, "RasterLayer") && !inherits(im, "RasterStack") && !inherits(im, "RasterBrick") && !inherits(im, "list")){
    stop("'im' must be a raster object or a list")
  } else if (inherits(im, "RasterLayer") || inherits(im, "RasterStack") || inherits(im, "RasterBrick")){
    im <- terra::rast(im)
  } else if (inherits(im, "list")){
    invisible(lapply(im, function(x) if(!inherits(x, "SpatRaster")) stop("all the elements of the list must be SpatRaster objects")))
    im <- terra::rast(im)
  }
  return(im)
}
