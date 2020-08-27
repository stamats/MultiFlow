# MultiFlow
The repository includes the development version of R package MultiFlow

[![License: LGPL v3](https://img.shields.io/badge/License-LGPL%20v3-blue.svg)](https://www.gnu.org/licenses/lgpl-3.0)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

## Installation

At the moment a patched version of package ShinyImage is required, which 
can be obtained from me on request.

The package requires Bioconductor package EBImage, which should be installed
first via

```{r, eval = FALSE}
## Install package BiocManager
if(!require(BiocManager)) install.packages("BiocManager")
## Use BiocManager to install EBImage
BiocManager::install("EBImage", update = FALSE)
```

Next, one can install package MultiFlow, where all remaining dependencies will
be installed automatically.

```{r, eval = FALSE}
## Install package remotes
if(!require(remotes)) install.packages("remotes")
## Install package MultiFlow
remotes::install_github("stamats/MultiFlow", build_vignettes = TRUE)
```

## Start App

```{r}
MultiFlow::runMultiFlowApp()
```


## Open User's Guide

```{r}
vignette("MultiFlow")
```

See also: [MultiFlow User's Guide](https://stamats.github.io/MultiFlow/MultiFlow.html).


## Description
Image analysis (cropping, segmetation, thresholding, feature engineering) 
of images taken from lateral flow assays, computation of linear calibration 
models, automatic report generation via rmarkdown by means of a shiny app.

![MultiFlow Shiny App](MultiFlowShinyApp.png)
