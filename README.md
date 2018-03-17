## Edits to functions `visweb` and `plotweb` from the R package [bipartite](https://github.com/biometry/bipartite)

Original code for functions `visweb` and `plotweb` was taken from [bipartite GitHub repository](https://github.com/biometry/bipartite). I made a fork initially, but I realized soon than the repository is taking more space that I am willing to allocate. 

All in all, I am only interested in editing the two functions mentioned above for personal use, so I started this separate repository.

[bipartite](https://github.com/biometry/bipartite) is licensed under GPL (mentioned [here]( https://github.com/biometry/bipartite/blob/c22e6f60d467b57e0bf5f4646f264f2867571543/bipartite/DESCRIPTION)) and this trickles down to this repository.

### Additional features:

#### in `plotweb`

- [x] disable `par` modifications during function call;
- label adjustments:
- [x] allow label size separately for lower and upper trophic levels;
- [x] allow text rotation separately for lower and upper labels;
- [x] allow adjustment of `pos`, `offset` and `font` separately for lower and upper labels;

#### in `visweb`

- [x] disable `par` modifications during function call;
- [x] add `x.lim` & `y.lim` arguments used for the same purpose as in `plotweb`;
- label adjustments:
  - [x] allow `side`, `las`, `font` and `mgp` changes for axis labels;
  - [x] add `mtext` for axis titles;
  - [ ] better replace `mtext` with `text`, which allows text rotation with `srt`; see this [FAQ](https://cran.r-project.org/doc/FAQ/R-FAQ.html#How-can-I-create-rotated-axis-labels_003f) or this [blog post](https://menugget.blogspot.de/2014/08/rotated-axis-labels-in-r-plots.html);
  - [ ] use also `text` for axis labels instead of the current `axis` (for the same reason as above);
  - [x] re-arrange code (plot first matrix, then annotation) - this helps detecting if labels are under the matrix when testing edits;
  - [ ] allow different sorting of the matrix to get the nestedness visual effect in any desired corner. This can be helpful when needing to rotate/further edit graphs in Inkscape.
