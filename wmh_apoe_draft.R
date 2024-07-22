# July 20, 2024
# Rachel LeMay

# first analysis: APOE-WMH 2-way ANOVA

#----------------------------------------------------------

setwd("/Users/rachellemay/Desktop/star_u_analyses")
wmh_dat <- read.csv("/Users/rachellemay/Desktop/star_u_analyses/ABCDS_APOE_df.csv")
wmh_dat

head(wmh_dat)
str(wmh_dat)


library(tidyverse)
library(psych)
library(car)
library(effectsize)

options(max.print = 10000)

names(wmh_dat)

psych::describe(wmh_dat)
aggregate( Sum.ROI ~ allele_combo, data = wmh_dat, FUN = mean, 
           filter(allele_combo == E4))
## aggregate (dependent variable, independent variable, data = name, FUN = mean)


dplyr::filter(wmh_dat, grepl('E4', allele_combo), !grepl('E2/E4', allele_combo), !grepl('E4/E2', allele_combo))
## so there's 72 pple w/ E4 total but excluding those carrying E2 w E4 bc that could confound results


