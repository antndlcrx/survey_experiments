#### main file for survey experiments analysis ####

# does data cleaning, calculation of weigts for analysis, and individual survey analyses

#### Set Up ####
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(tidyverse, readr, broom, 
               lubridate, gt, gtsummary, survey, readxl,
               gridExtra, knitr, haven, nnet, stargazer, kableExtra,
               gplots,mice, xtable)

Sys.getlocale()
# Set this to a locale that supports Cyrillic, for example, on Windows
Sys.setlocale("LC_ALL", "Russian")


#### load data ####

survey_march <- read_csv("data/survey_march_post_election.csv",
                         locale = locale(encoding = "UTF-8"))
survey_feb <- read_xlsx("data/survey_feb.xlsx")
survey_aug <- read_csv("data/survey_aug.csv",
                       locale = locale(encoding = "UTF-8"))
survey_july <- read_xlsx("data/survey_june_july.xlsx") 
survey_sept <- read_xlsx("data/survey_sept_24.xlsx")
levada_omnibus <- read_sav("data/levada_omnibus")


# population frame for weights
ru_population_frame <- read_csv("data/ru_population_frame.csv")[-c(1:3,16:18), -c(3,7)] # remove cats before 18 years old and totals


#### clean data ####
source("data_cleaning.R", encoding = "UTF-8")

#### create survey weights #####
source("add_weights.R", encoding = "UTF-8")

#### run analyses ####

## survey july ##
# includes randomisation check, binary and continous versions, and multiple imputation version
source("analysis_july.R", encoding = "UTF-8")

## survey September ##
# includes randomisation check, weighted and unweighted versions, and multiple imputation version
source("analysis_september.R", encoding = "UTF-8")

## survey levada 2021 ##
source("analysis_levada.R", encoding = "UTF-8")

## survey march ##
source("analysis_march.R", encoding = "UTF-8")
