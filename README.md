
<!-- README.md is generated from README.Rmd. Please edit that file -->

# styler.equals

This package is based on
<https://github.com/lorenzwalthert/styler.yours>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/robinlovelace/styler.equals/workflows/R-CMD-check/badge.svg)](https://github.com/robinlovelace/styler.equals/actions)
<!-- badges: end -->

The goal of {styler.equals} is to provide an implementation of the
‘equals style’ used by Yuhei Xie, Colin Gillespie and many in the
[\#rspatial](https://twitter.com/hashtag/rspatial) community.

So far it basically just the ‘tidyverse style’ but with equals
assignment. In the future it may evolve, e.g. to implement the
[Geocomputation with R style
guide](https://github.com/Robinlovelace/geocompr/blob/main/misc/our-style.md).

It is a third-party style guide for
[{styler}](https://styler.r-lib.org).

## Installation

You can install the released version of {styler.equals} from
[GitHub](https://github.com) with:

``` r
remotes::install_github("robinlovelace/styler.equals")
```

## Example

This is a basic example of how to style code with it.

``` r
library(styler.equals)
cache_deactivate()
#> Deactivated cache.
text = "x <- 4
y = 3
a;
"

text_styled_equals = style_text(text)
text_styled_equals
#> x = 4
#> y = 3
#> a;
```
