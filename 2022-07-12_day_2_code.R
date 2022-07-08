################################################################################
# Name: Intro to Data Wrangling, Exploration, and Analysis with R Day 2
# Author: Nina Brooks 
# Date: July 12, 2022
################################################################################

################################################################################
# Load Packages #
################################################################################
# load packages
library(readxl) # this package allows you to read .xlsx files into R
library(haven) # this package allows you to read & write stata .dta files
library(tabulator) # this is a package that is useful for making cross-tabs
library(janitor) # contains useful commands for data cleaning
library(palmerpenguins) # provides a dataset on penguins we'll use
library(glue) # tidyverse-adjacent package makes working with strings simpler
library(lubridate) # tidyverse package for working with dates
library(broom) # for tidying regression results
library(infer) # a tidyverse package for inference
library(gtsummary) # for summary & regression tables
library(fixest) # for fixed effects regression (among other things)
library(marginaleffects) # for marginal effects after regression
library(modelsummary) # for summary & regression tables
library(hrbrthemes) # additional themes to supplement ggplot2
library(patchwork) # for combining ggplots together
library(ggsci) # additional color palettes for ggplots
library(tidyverse) # this loads the 8 core packages of the tidyverse
library(tidylog) # adds extra explanation of tidyverse commands



################################################################################
# 
################################################################################