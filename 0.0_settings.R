##### my packages ################################################################################
## CRAN
cran.packages <- c('ggplot2','gridExtra','viridis','dplyr','plyr','reshape2','lubridate','tidyverse')
install.this <- cran.packages[!(cran.packages %in% utils::installed.packages()[,"Package"])]
if(length(install.this)>1) install.packages(install.this)
dummy <- lapply(cran.packages, require, character.only = TRUE)


## github
git.packages <- c('catchR','DFOdata')
install.this <- git.packages[!(git.packages %in% utils::installed.packages()[,"Package"])]
if('catchR' %in% install.this)  devtools::install_github("iml-assess/catchR")
if('DFOdata' %in% install.this)  devtools::install_github("im-assess/DFOdata")
dummy <- lapply(git.packages, require, character.only = TRUE)

##### my directories ################################################################################
dir.dat  <- 'data/'
dir.Rdata <- 'Rdata/'

##### source src directory  ################################################################################
invisible(sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source))

##### my ggplot theme ################################################################################
theme_set(theme_mackerel())             # theme_mackerel from catchR

##### passwords databases #############################################################################
source('passwords.R')
