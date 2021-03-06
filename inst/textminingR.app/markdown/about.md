<!-- README.md is generated from README.Rmd. Please edit that file -->

### textminingR

Developed and maintained by Mikyung Shin

-   <mikyung.shin@wtamu.edu>
-   <https://mshin77.net>

[Source code available on
Github](https://github.com/mshin77/textminingR)

`textminingR` provides a supporting workflow for text mining analysis.
The web app incorporates [quanteda](https://github.com/quanteda/quanteda) (preprocess), [stm](https://github.com/bstewart/stm) (structural topic modeling), and [ggraph](https://github.com/thomasp85/ggraph) as well as [widyr](https://github.com/dgrtwo/widyr) (network analysis). [tidytext](https://github.com/cran/tidytext) was implemented to tidy non-tidy format objects. 

### Installation

The development version from [GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("mshin77/textminingR")
```

### Example

Launch and browser the textminingR app:

``` r
library(textminingR)
textminingR.app()
```
