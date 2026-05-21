# venny

`venny` is an R package for gene expression analysis with a focus on Venn diagram visualization (UpSet plot support not yet available).  
It offers a simple and efficient way to extract target genes after DEG analysis, and can produce publication-ready plots out of the box.  
For more advanced or customizable visualizations, consider other packages such as `ggVennDiagram`, `ggvenn`, or `UpSetR`.

## Installation

You can install the development version of `venny` from [GitHub](https://github.com/P10911004-NPUST/venny) with:

``` r
if ( ! require(devtools) ) install.packages("devtools")
devtools::install_github("P10911004-NPUST/venny")
```

## Quick start

```r
venny(LGL23$DEGs)
```