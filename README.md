## Edits to functions `visweb` and `plotweb` from the R package [bipartite](https://github.com/biometry/bipartite)

Note the naming equivalence:

- `plot_grid` corresponds to `bipartite::visweb` ([Aug 19, 2015][visweb])
- `plot_bipartite` corresponds to `bipartite::plotweb` ([Oct 6, 2014][plotweb])

Original code for functions [`visweb`][visweb] (`plot_grid`) and [`plotweb`][plotweb] (`plot_bipartite`) was taken from [bipartite GitHub repository](https://github.com/biometry/bipartite). I made a fork initially, but I realized soon than the repository is taking more space that I am willing to allocate. 

All in all, I am only interested in editing the two functions mentioned above for personal use, so I started this separate repository.

[visweb]: https://github.com/biometry/bipartite/blob/81fd6a4c181eff2fc1d229f32c2f73f91fb37c87/bipartite/R/visweb.R
[plotweb]: https://github.com/biometry/bipartite/blob/81fd6a4c181eff2fc1d229f32c2f73f91fb37c87/bipartite/R/plotweb.R

## Usage

This is work in progress!

```r
library(bipartite)
# Source the functions from this repository
source("https://raw.githubusercontent.com/valentinitnelav/bipartite_webs/master/R/plot_bipartite.R")
source("https://raw.githubusercontent.com/valentinitnelav/bipartite_webs/master/R/plot_grid.R")
```

## Edits / Additional features:

### in `plot_bipartite` (`bipartite::plotweb`)

- [x] disable `par` modifications during function call;
- label adjustments:
- [x] allow label size separately for lower and upper trophic levels;
- [x] allow text rotation separately for lower and upper labels;
- [x] allow adjustment of `pos`, `offset` and `font` separately for lower and upper labels;

### in `plot_grid` (`bipartite::visweb`)

- [x] disable `par` modifications during function call;
- [x] add `x.lim` & `y.lim` arguments used for the same purpose as in `plotweb`;
- label adjustments:
  - [x] allow `side`, `las`, `font` and `mgp` changes for axis labels;
  - [x] add `mtext` for axis titles;
  - [ ] better replace `mtext` with `text`, which allows text rotation with `srt`; see this [FAQ](https://cran.r-project.org/doc/FAQ/R-FAQ.html#How-can-I-create-rotated-axis-labels_003f) or this [blog post](https://menugget.blogspot.de/2014/08/rotated-axis-labels-in-r-plots.html);
  - [ ] use also `text` for axis labels instead of the current `axis` (for the same reason as above);
  - [x] re-arrange code (plot first matrix, then annotation) - this helps detecting if labels are under the matrix when testing edits;
  - [ ] allow different sorting of the matrix to get the nestedness visual effect in any desired corner. This can be helpful when needing to rotate/further edit graphs in Inkscape.
  - [ ] implement label/species coloring

## License

The package [bipartite](https://github.com/biometry/bipartite) is licensed under GPL (mentioned [here](https://github.com/biometry/bipartite/blob/c22e6f60d467b57e0bf5f4646f264f2867571543/bipartite/DESCRIPTION)) and this trickles down to this repository.