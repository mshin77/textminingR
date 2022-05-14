
<!-- README.md is generated from README.Rmd. Please edit that file -->

# textminingR

<!-- badges: start -->
<!-- badges: end -->

`textminingR` provides a supporting workflow for text mining analysis.
The web app incorporates
[quanteda](https://github.com/quanteda/quanteda) (preprocess),
[stm](https://github.com/bstewart/stm) (structural topic modeling), and
[ggraph](https://github.com/thomasp85/ggraph) as well as
[widyr](https://github.com/dgrtwo/widyr) (network analysis).
[tidytext](https://github.com/cran/tidytext) was implemented to tidy
non-tidy format objects.

## Installation

You can install the released version of textminingR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("textminingR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("mshin77/textminingR")
```

## Example

Launch and browser the textminingR app:

``` r
library(textminingR)
textminingR.app()
#> 
#> Listening on http://127.0.0.1:3363
#> [1] ""
#> [1] 0
#> attr(,"class")
#> [1] "integer"                "shinyActionButtonValue"
```
