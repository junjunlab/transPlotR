
# transPlotR

<!-- badges: start -->
<!-- badges: end -->

There  are some packages to plot gene structures, for example [**ggbio**](https://bioconductor.org/packages/release/bioc/html/ggbio.html), [**ggtranscript**](https://github.com/dzhang32/ggtranscript)... But there are still some limitations for them. The **IGV** software provides a good visualization for gene multiple isoforms. If you want to plot **protein-coding** or **non-coding genes**, it seems a little bit difficult for you to draw with a lot of codes. Here I developed a small R package named [**transPlotR**](https://github.com/junjunlab/transPlotR) which make gene structure visualization much easier. You can provide a little parameters to **trancriptVis** to make a plot with your own **GTF** files.

## Installation

You can install the development version of transPlotR like so:

``` r
# install.packages("devtools")
devtools::install_github("junjunlab/transPlotR")
```

## Example

This is a basic example:

``` r
library(transPlotR)
## basic example code

data(gtf)

# non-coding gene
trancriptVis(gtfFile = gtf,
             gene = 'Xist')
```

![image](https://user-images.githubusercontent.com/64965509/178003174-a272c28d-d1fb-49e5-9c85-c427b83982f3.png)

## More examples

> - **https://github.com/junjunlab/transPlotR/wiki/TransPlot-documentation**
