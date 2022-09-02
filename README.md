
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

# facet by gene
trancriptVis(gtfFile = gtf,
             gene = c('Camk1g','Daw1','Oprk1'),
             facetByGene = T)
```

![image](https://user-images.githubusercontent.com/64965509/188102988-fd13646d-46d8-4f47-9921-990815f8d376.png)

## More examples

> - **https://github.com/junjunlab/transPlotR/wiki/TransPlot-documentation**

> - **https://github.com/junjunlab/transPlotR/wiki/TransPlot-0.0.3-documentation**
