# MultiFlow
The repository includes the development version of R package MultiFlow


## Installation

```{r, eval = FALSE}
# install.packages("remotes")
remotes::install_github("stamats/MultiFlow")
```

## Start App

```{r}
MultiFlow::runMultiFlowApp()
```

## Description
Image analysis (cropping, segmetation, thresholding, feature engineering) 
of images taken from lateral flow assays, computation of linear calibration 
models, automatic report generation via rmarkdown by means of a shiny app.

![MultiFlow Shiny App](MultiFlowShinyApp.png)
