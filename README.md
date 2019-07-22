# shufflecards <img src="man/figures/shufflecards.png" width=200 align="right" />

> Filter and sort grid layouts in Shiny application and Markdown document with [Shuffle.js](https://github.com/Vestride/Shuffle).


[![Travis-CI Build Status](https://travis-ci.org/dreamRs/shufflecards.svg?branch=master)](https://travis-ci.org/dreamRs/shufflecards)
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)


![](man/figures/shufflecards.gif)



## Installation

You can install `shufflecards` from GitHub:

```r
remotes::install_github("dreamRs/shufflecards")
```


## Usage

**Markdown:** use `shuffle_widget` to create a grid of elements, arrange the grid with buttons and filter with `crosstalk` inputs.

**Shiny:** use `shuffle_container` to create a grid of elements in UI, use classic Shiny inputs and server-side logic to arrange & filter the grid.



## Examples


### Markdown

* With `htmlwidget` [`billboarder`](https://github.com/dreamRs/billboarder) : https://dreamrs.github.io/tweets-transports/
* With HTML tags : https://dreamrs.github.io/shufflecards/playing-cards
* With [`flexdashboard`](https://rmarkdown.rstudio.com/flexdashboard/index.html) and [`highcharter`](http://jkunst.com/highcharter/index.html) : https://dreamrs.github.io/shufflecards/flexdashboard
* With [`ggplot2`](https://ggplot2.tidyverse.org/) : https://dreamrs.github.io/shufflecards/gapminder-md-ggplot2


### Shiny

You can run the playing cards example with:
```r
shufflecards::play()
```

With data from `gapminder` package and `ggplot2`:

![](man/figures/shufflecards-gapminder.gif)

Run this example with :

```r
run_example(example = "gapminder")
```


## Related package

* [trelliscopejs R Package](https://github.com/hafen/trelliscopejs) : Trelliscope is a scalable, flexible, interactive approach to visualizing data.

