
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cblindplot

<!-- badges: start -->

[![main](https://github.com/ducciorocchini/cblindplot/actions/workflows/main.yaml/badge.svg)](https://github.com/ducciorocchini/cblindplot/actions/workflows/main.yaml)
<!-- badges: end -->

**cblindplot** R package is described in Rocchini et al. -
<https://doi.org/10.1016/j.ecoinf.2023.102045>.

<p align="center">
  <img 
    src="https://github.com/user-attachments/assets/857b6755-8f7d-4a49-a123-92332de2fc76"
    alt="cellulaR"
    width="300"
  />
</p>

## Installation

You can install the development version of **cblindplot** from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ducciorocchini/cblindplot")
```

## Example

``` r
library(cblindplot)
my_image <- system.file("pic/imager.png", package = "cblindplot")
```

``` r
my_image_terra <- terra::rast(my_image)
terra::plotRGB(my_image_terra)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
cblind.plot(my_image, cvd = "deuteranopia")
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

## Citation

To cite the **cblindplot** package in publications, please use [this
paper](https://doi.org/10.1016/j.ecoinf.2023.102045):

> Rocchini, D., Nowosad, J., D’Introno, R., Chieffallo, L., Bacaro, G.,
> Gatti, R. C., Foody, G. M., Furrer, R., Gabor, L., Malavasi, M.,
> Marcantonio, M., Marchetto, E., Moudry, V., Ricotta, C., Simova, P.,
> Torresani, M., & Thouverai, E. (2023). Scientific maps should reach
> everyone: The cblindplot R package to let colour blind people
> visualise spatial patterns. Ecological Informatics.
> <https://doi.org/10.1016/j.ecoinf.2023.102045>

LaTeX/BibTeX version can be obtained with:

    library(cblindplot)
    citation("cblindplot")
