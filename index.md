
# MrBean <img src="man/figures/hex-MrBean.png" width="120px" align="right"/>


[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)


Mr. Bean is an easy to use R-Shiny web-app that simplifies the analysis of large-scale plant breeding experimental analysis by using the power and versatility of Linear Mixed Models (LMM). This app combines the analytical robustness and speed of ASReml and SpATS with the visual power offered by R.  Mr. Bean provides a graphical workflow for importing data, identifying outliers, and fitting field data using LMM with or without spatial correction. The results are BLUPs/BLUEs predictions and heritabilities for single-environmental experiments or multiple-environmental trial (MET) analysis. In addition, Mr. Bean also provides a module for exploring results from METs using several graphical and multivariate techniques.

<a href="https://mrpackages.netlify.app/">https://mrpackages.netlify.app/</a> <br>
<a href="https://apariciojohan.github.io/MrBeanApp/">https://apariciojohan.github.io/MrBeanApp/</a> 


## Installation

You can install the package:

``` r
devtools::install_github("AparicioJohan/MrBeanApp")                            
```
or

```r
remotes::install_github("AparicioJohan/MrBeanApp")                           
```


## Example

``` r
library(MrBean)
run_app()
```


## Demo

A running demo is on [shinyapps.io](https://beanteam.shinyapps.io/MrBean_BETA/).

<div class="row">
<div class="card">
<a href="https://beanteam.shinyapps.io/MrBean_BETA/" target="_blank"><img src="man/figures/desktop_update.png"></a>
</div>
</div>

<br>

Please note that the ‘MrBean’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
