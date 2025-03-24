
# Replication: “Simultaneous Inference Bands for Autocorrelations”

This repository contains replication files for the paper “Simultaneous
Inference Bands for Autocorrelations” by Uwe Hassler, Marc-Oliver Pohle
and Tanja Zahn. The `R` package accompanying the paper is available
under the repository [ACFbands](https://github.com/TanjaZahn/ACFbands).

## Installation

Before running the replication code, the accompanying `R` package
[ACFbands](https://github.com/TanjaZahn/ACFbands) has to be installed
from Github:

``` r
install.packages("devtools")
library(devtools)
install_github("TanjaZahn/ACFbands")
```

To install the other packages used in the replication files, run the
code:

``` r
install.packages("tidyverse")
install.packages("lubridate")
install.packages("patchwork")
install.packages("stargazer")
install.packages("xtable")
install.packages("kableExtra")
```

## code

The folder “code” includes the `R` code that was used to create all the
results in the paper. It contains several subfolders. Within the
subfolder, `R` scripts are numerated in the respective order. We give a
short overview:

- **analytical_examples:** contains the files that are used to show the
  properties of the inference bands for the example of an AR(1) process
  (Figures 3 to 7 as well as the examples for the B matrices contained
  in the Appendix).

- **application:** contains the files used in the empirical application
  in section 7 and Appendix E.

- **sim_ts:** produces the simulation results for time series,
  i.e. sections 6.1 and 6.2.

- **sim_dyn:** produces the simulation results for dynamic regressions,
  i.e. section 6.3.

- **functions:** contains some custom functions that files from the
  other folders use. They will be called upon automatically in the
  respective places. Thus, the files contained in “functions” do not
  have to be run explicitly.

- **mytheme:** contains some graphical settings for the plots. This file
  does not have to be run explicitly.

*Note*: Most of the code in **sim_ts** and **sim_dyn** checks if
simulation results are already available in the folder **results** and
is only executed, if they are not.

## data

This folder contains the data used in the empirical application. It has
been downloaded from the [FRED-MD
database](https://www.stlouisfed.org/research/economists/mccracken/fred-databases),
see also the [working
paper](https://s3.amazonaws.com/real.stlouisfed.org/wp/2015/2015-012.pdf)
by McCracken and Ng (2016) for a description.

## results

The raw results from the simulation runs are stored in the folder
“results” for the time series case (“sim_ts”) and for dynamic regression
residuals (“sim_ts”).

## graphics

This folder contains all plots and tables.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-mccracken2016" class="csl-entry">

McCracken, Michael W, and Serena Ng. 2016. “FRED-MD: A Monthly Database
for Macroeconomic Research.” *Journal of Business & Economic Statistics*
34: 574–89.

</div>

</div>
