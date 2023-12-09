library(dplyr)
library(tidyverse)
library(readxl)
library(countrycode)

############What do I want to do? ############

############ I want to remake the load in the data for each year, turn it into a list.

############ I want countries to be in ISOCODE2c


# Set wd

setwd("C:/Users/Viktor/Documents/Stockholms Universitet/Statsvet Kandidiat/Statistik analys/UN_Negotiations/CCW/CCW_Analysis")


############# 2020 ##############

Oxf_AI_2020 <- read_excel("~/Stockholms Universitet/Statsvet Kandidiat/Statistik analys/AI readiness/2020-Government-AI-Readiness-Index-public-dataset.xlsx")

## Lose ranking column

Oxf_AI_2020$`2020 Ranking` <- NULL

## Rename to iso-code 2 c

Oxf_AI_2020$Country <- countrycode(Oxf_AI_2020$Country, "country.name", "iso2c")

View(Oxf_AI_2020)

#Renaming column from score to AI_Index

names(Oxf_AI_2020)[names(Oxf_AI_2020) == "Score"] <- "AI_Index"


###############################################################

########## 2021 #################

Oxf_AI_2021 <- read_excel("~/Stockholms Universitet/Statsvet Kandidiat/Statistik analys/AI readiness/2021-Government-AI-Readiness-Index-public-dataset.xlsx")
view(Oxf_AI_2021)
## Lose ranking column

Oxf_AI_2021$`2021 Ranking` <- NULL
view(Oxf_AI_2021)
## Rename to iso-code 2 c

Oxf_AI_2021$Country <- countrycode(Oxf_AI_2021$Country, "country.name", "iso2c")
view(Oxf_AI_2021)



names(Oxf_AI_2021)[names(Oxf_AI_2021) == "Total"] <- "AI_Index"

############################################################

######## 2023 ################

Oxf_AI_2023 <- read_excel("~/Stockholms Universitet/Statsvet Kandidiat/Statistik analys/AI readiness/2023-AI-Readiness-Index-public-dataset.xlsx")


## Lose ranking column

Oxf_AI_2023$`Ranking` <- NULL

## Rename to iso-code 2 c

Oxf_AI_2023$Country <- countrycode(Oxf_AI_2023$Country, "country.name", "iso2c")
view(Oxf_AI_2023)


names(Oxf_AI_2023)[names(Oxf_AI_2023) == "Total"] <- "AI_Index"
