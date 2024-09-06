
nncblind <- function(im, cvd = c("protanopia", "deuteranopia", "tritanopia"), r = 1, g = 2, b = 3) {

  #Controllo cvd
  cvd <- cvd[1]
  if(!cvd %in% c("protanopia", "deuteranopia", "tritanopia")) stop("Wrong 'cvd` value. It can be 'protanopia', 'deuteranopia', or 'tritanopia'")

  #Controllo immagine
  if (!inherits(im, "SpatRaster") && !inherits(im, "RasterLayer") && !inherits(im, "RasterStack") && !inherits(im, "RasterBrick") && !inherits(im, "list") && !inherits(im, "character")){
    stop("'im' must be a raster object, a list or a path to an image")
  } else if (inherits(im, "RasterLayer") || inherits(im, "RasterStack") || inherits(im, "RasterBrick") || is.character(im)){
    suppressWarnings(im <- terra::rast(im))
  } else if (inherits(im, "list")){
    invisible(lapply(im, function(x) if(!inherits(x, "SpatRaster")) stop("all the elements of the list must be SpatRaster objects")))
    suppressWarnings(im <- terra::rast(im))
  }

  # Preparazione dati
  df <- as.data.frame(im, xy = T)
  df_mod <- data.frame(df[,1], df[,2], df[,(r + 2)], df[,(g + 2)], df[,(b + 2)])
  colnames(df_mod) <- c("x", "y", "R", "G", "B")

  # Convertire in character i valori delle colonne R, G, B al fine di renderle compatibili con il modello nnet
  df_mod$R = as.character(R)
  df_mod$G = as.character(G)
  df_mod$B = as.character(B)

  # Richiamare il modello mdNNea
  load("R/sysdata.rda")
  mdNN <- mdnew

  # Predirre il colore e inserirlo nel dataframe
  colors <- stats::predict(mdNN, df_mod[, c(3,4,5)], type = "class")
  col <- cbind(df_mod, colors)

  # Divisione colori
  white <- col[col$colors == "white",]
  red <- col[col$colors == "red",]
  brown <- col[col$colors == "brown",]
  orange <- col[col$colors == "orange",]
  yellow <- col[col$colors == "yellow",]
  lightgreen <- col[col$colors == "lightgreen",]
  green <- col[col$colors == "green",]
  lightblue <- col[col$colors == "lightblue",]
  blue <- col[col$colors == "blue",]
  pink <- col[col$colors == "pink",]
  violet <- col[col$colors == "violet",]
  grey <- col[col$colors == "grey",]
  black <- col[col$colors == "black",]

  # Unire i colori
  unir <- rbind(white, red, orange, brown, yellow, lightgreen, green, lightblue, blue, violet, pink, grey, black)

  # Creare scala discreta

  # Creazione della colonna
  fin <- cbind(unir, values = "")

  # Mettere i numeri ai colori
  fin[fin$colors == "white", "values"] <- 1
  fin[fin$colors == "red", "values"] <- 2
  fin[fin$colors == "orange", "values"] <- 3
  fin[fin$colors == "brown", "values"] <- 4
  fin[fin$colors == "yellow", "values"] <- 5
  fin[fin$colors == "lightgreen", "values"] <- 6
  fin[fin$colors == "green", "values"] <- 7
  fin[fin$colors == "lightblue", "values"] <- 8
  fin[fin$colors == "blue", "values"] <- 9
  fin[fin$colors == "violet", "values"] <- 10
  fin[fin$colors == "pink", "values"] <- 11
  fin[fin$colors == "grey", "values"] <- 12
  fin[fin$colors == "black", "values"] <- 13

  # Ordinare i values
  fin$values = factor(values, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13"))

  # Ordinare la palette di colore da mostrare nelle immagini per le diverse malattie
  if(cvd == "deuteranopia") {
    # con la deuteranopia
    ord_pal <-  c("#FFFFFF", "#FF4040", "#FFC040", "#8B4513", "#FFFF00", "#88FF33", "#00FF00", "#CCFFFF", "#0000FF", "#660066", "#FFD1D9", "#808080", "#000000")

  } else if(cvd == "protanopia") {
    # con la protanopia
    ord_pal <-  c("#FFFFFF", "#FF4040", "#FFD280", "#8B4513", "#FFFF33", "#228B22", "#006600", "#CCFFFF", "#0000FF", "#993399", "#FFD1D9", "#808080", "#000000")

  } else if(cvd == "tritanopia") {
    # con la tritanopia
    ord_pal <-  c("#FFFFFF", "#FF0000", "#FFD280", "#8B4513", "#CCCC00", "#228B22", "#008000", "#CCCCCC", "#7FFF7F", "#FF0000", "#FF4040", "#808080", "#000000")

  }

  # Creazione del ggplot
  ggt <- ggplot2::ggplot(fin, ggplot2::aes(x = x, y = y, fill = values)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_manual(values = ord_pal[unique(fin$values)], na.value = "transparent") +
    ggplot2::coord_sf() +
    ggplot2::theme(legend.position = "bottom")

  return(ggt)

}
