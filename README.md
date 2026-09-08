# RFtrim.R

Adaptively trimmed random forests, as described in "Alpha-Trimming: Locally Adaptive Tree Pruning for Random Forests," submitted to *Computational Statistics & Data Analysis*.

**Note:** Almost the entire source code is based on a version of the `ranger` package on CRAN, which is under the [GPL-3](https://cran.r-project.org/web/licenses/GPL-3) license.
A few modifications are made to allow additional data to be stored during tree construction,
which is used later on during the alpha-trimming phase.

## Installation

```r
# Install from source
devtools::install_github("nikola-sur/RFtrim.R-CSDA")
```

## See also

- [AlphaTrim-Exp](https://github.com/nikola-sur/AlphaTrim-Exp-CSDA): Simulation scripts to reproduce the experiments in the paper.
