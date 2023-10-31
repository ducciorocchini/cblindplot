# Code related to the manuscript:
# Scientific maps should reach everyone: a straightforward approach to let colour blind people visualise spatial patterns
# by Duccio Rocchini et al.
# Original code by: Elisa Thouverai rewritten, readapted, and expanded by Jakub Nowosad

#' Color Blind Plot
#'
#' It updates the color palette on an input image and returns a new visualization along with a new color legend.
#'
#' @param im A file path to an image or a terra/raster object
#' @param cvd A type of color vision deficiency (CVD): "protanopia", "deuteranopia", or "tritanopia";
#'   or a vector of colors
#' @param r Index of the Red channel
#' @param g Index of the Green channel
#' @param b Index of the Blue channel
#' @param crop_manual Do you want to manually crop the input image?
#' @param select_class Do you want to select only certain colors in the image for the further processing?
#' @param legend Optional: a file path to a legend image or a terra/raster object with a legend of im
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#' my_image <- system.file("pic/imager.png", package = "cblindplot")
#'
#' #suppressWarnings(my_image_terra <- terra::rast(my_image))
#' #terra::plotRGB(my_image_terra)
#'
#' cblind.plot(my_image)
#' #cblind.plot(my_image, cvd = "tritanopia")
#' #cblind.plot(my_image, cvd = hcl.colors(7, palette = "Sunset"))
#' #cblind.plot(my_image, crop_manual = TRUE)
#' #cblind.plot(my_image, select_class = TRUE)
#'
#' #my_legend <- system.file("pic/legendr.png", package = "cblindplot")
#' #cblind.plot(my_image, cvd = "tritanopia", legend = my_legend)
#' #cblind.plot(my_image, cvd = hcl.colors(7, palette = "Sunset"), legend = my_legend)
cblind.plot = function(im, cvd, r = 1, g = 2, b = 3, crop_manual = FALSE, select_class = FALSE,
                       legend){
  if (missing(cvd)){
    cvd <- "protanopia"
  } else if(length(cvd) == 1 && !cvd %in% c("protanopia", "deuteranopia", "tritanopia")){
    stop("Wrong 'cvd` value. It can be 'protanopia', 'deuteranopia', 'tritanopia', or a vector of colors")
  }
  im <- cblind.prep.input(im)
  if (missing(legend)){
    impl <- cblind.prep(im, r = r, g = g, b = b, crop_manual = crop_manual, select_class = select_class)
    impl <- as.data.frame(impl, xy = TRUE)[c(1:3)]
    colnames(impl) <- c("x", "y", "values")
    ggt <- ggplot2::ggplot(impl) +
      ggplot2::geom_raster(ggplot2::aes_string(x = "x", y = "y", fill = "values")) +
      ggplot2::coord_equal() +
      ggplot2::theme_void()
    if (length(cvd) > 1){
      pl <- ggt + ggplot2::scale_fill_gradientn(colours = cvd, na.value = "transparent")
    } else if(cvd == "deuteranopia") {
      pl <- ggt + ggplot2::scale_fill_viridis_c(na.value = "transparent")
    } else if(cvd == "protanopia") {
      pl <- ggt + ggplot2::scale_fill_viridis_c(na.value = "transparent", option = "E")
    } else if(cvd == "tritanopia") {
      pl <- ggt + ggplot2::scale_fill_viridis_c(na.value = "transparent", option = "A")
    }
  } else {
    legend <- cblind.prep.input(legend)
    impl <- cblind.prep.legend(im, cvd = cvd, legend = legend)
    impl <- as.data.frame(impl, xy = TRUE)[c(1:5)]
    colnames(impl) <- c("x", "y", "r", "g", "b")
    impl$hexes = grDevices::rgb(impl[, 3:5], maxColorValue = 255)
    pl <- ggplot2::ggplot(impl) +
      ggplot2::geom_raster(ggplot2::aes_string(x = "x", y = "y", fill = "hexes")) +
      ggplot2::coord_equal()  +
      ggplot2::scale_fill_identity() +
      ggplot2::theme_void()
  }
  return(suppressWarnings(print(pl)))
}

#' Prepare Input Image for Color Blind Plot
#'
#' Prepares input image for cblind.plot by: a) cropping the image, b) selecting certain colors, c) performing PCA on the image.
#'
#' @param im A file path to an image
#' @param r Index of the Red channel
#' @param g Index of the Green channel
#' @param b Index of the Blue channel
#' @param crop_manual Do you want to manually crop the input image?
#' @param select_class Do you want to select only certain colors in the image for the further processing?
#'
#' @return A terra SpatRaster object
#' @export
#'
#' @examples
#' my_image <- system.file("pic/imager.png", package = "cblindplot")
#' #suppressWarnings(my_image_terra <- terra::rast(my_image))
#' #terra::plotRGB(my_image_terra)
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
      im2 <- terra::deepcopy(im)
      print(coords)
      im <- terra::crop(im2, terra::ext(coords))
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
    impca <- im[[c(r, g, b)]]
    pca_model <- stats::prcomp(stats::na.omit(as.matrix(impca)))
    impl <- terra::predict(impca, pca_model)
  }

  return(impl)
}

cblind.prep.legend = function(im, cvd, legend){
  # extract colors from the old legend --------------------------------------
  samples <- terra::spatSample(legend, size = 100, xy = TRUE)
  if (nrow(legend) > ncol(legend)){
    rgbs <- samples[order(samples$y), 3:5]
  } else {
    rgbs <- samples[order(samples$x), 3:5]
  }
  hexs <- grDevices::rgb(rgbs, maxColorValue = 255)
  unique_cols <- unique(hexs)

  if (length(cvd) > 1){
    new_small_hexs_fun <- grDevices::colorRampPalette(cvd)
    new_small_hexs <- new_small_hexs_fun(length(unique_cols))
  } else if(cvd == "deuteranopia") {
    new_small_hexs <- grDevices::hcl.colors(n = length(unique_cols), palette = "viridis")
  } else if(cvd == "protanopia") {
    new_small_hexs <- grDevices::hcl.colors(n = length(unique_cols), palette = "cividis")
  } else if(cvd == "tritanopia") {
    new_small_hexs <- viridisLite::magma(length(unique_cols))
  }

  # create new colors -------------------------------------------------------
  new_small_rgbs <- t(grDevices::col2rgb(new_small_hexs))

  # recalculate values of the original raster -------------------------------
  all_vals <- terra::values(im)
  which_not_na <- as.logical(as.vector(terra::values(terra::noNA(im))))
  all_hexs <- rep(NA, nrow(all_vals))
  all_hexs[which_not_na] <- grDevices::rgb(all_vals[which_not_na, ], maxColorValue = 255)
  ids <- seq_along(all_hexs)
  new_vals <- merge(cbind.data.frame(all_vals, all_hexs, ids), cbind.data.frame(unique_cols, new_small_rgbs),
                    by.x = "all_hexs", by.y = "unique_cols",
                    sort = FALSE, all = TRUE)
  new_vals <- new_vals[order(new_vals$ids), ]

  # get results -------------------------------------------------------------
  my_image_new <- terra::deepcopy(im)
  terra::values(my_image_new) <- new_vals[, 6:8]
  return(my_image_new)
}

cblind.prep.input = function(im){
  if (!inherits(im, "SpatRaster") && !inherits(im, "RasterLayer") && !inherits(im, "RasterStack") && !inherits(im, "RasterBrick") && !inherits(im, "list") && !inherits(im, "character")){
    stop("'im' must be a raster object, a list or a path to an image")
  } else if (inherits(im, "RasterLayer") || inherits(im, "RasterStack") || inherits(im, "RasterBrick") || is.character(im)){
    suppressWarnings(im <- terra::rast(im))
  } else if (inherits(im, "list")){
    invisible(lapply(im, function(x) if(!inherits(x, "SpatRaster")) stop("all the elements of the list must be SpatRaster objects")))
    suppressWarnings(im <- terra::rast(im))
  }
  return(im)
}
