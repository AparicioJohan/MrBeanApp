
# MrBean



[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)


The goal of MrBean is to analyze field experiments with a user
interface.

## Installation

You can install the package:

``` r
devtools::install_bitbucket("johanaparicio/mrbean", auth_user = "AparicioJohan", password = rstudioapi::askForPassword() )
```
or

```r
remotes::install_bitbucket("johanaparicio/mrbean", auth_user = "AparicioJohan", password = rstudioapi::askForPassword() )
```


## Example

``` r
library(MrBean)
run_app()
```

Please note that the ‘MrBean’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
