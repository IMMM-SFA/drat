# drat
De-Rating Available (Power)


Running on Deception:
* load modules:
  * `module load gcc/11.2.0`
  * `module load R/4.0.2`
* install `renv`:
  * `R`
  * `install.packages('renv')`
* initialize the renv (or restore from lock file):
  * `renv::init()`
  * `quit()`
  * Alternatively, to restore from environment lock file:
    * `renv::restore()`
    * `quit()`
* install dependencies (if you did not restore):
  * `R`
  * I encountered some difficulties installing `rstan` and `rstanarm` and had to proceed in this order:
  * `install.packages('Rcpp')`
  * `install.packages('rstan')`
  * `install.packages(c('tidyverse', 'lmerTest', 'sjstats', 'data.table', 'dplyr', 'feather', 'tibble', 'ggplot2'))`
  * `install.packages('rstanarm', type = 'source')`
  

