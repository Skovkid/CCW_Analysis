#install.packages("wbstats")
#install.packages("devtools")
#install.packages("wb")
#devtools::install_github("nset-ornl/wbstats")
#install.packages("WDI")

library(tidyverse)
library(wbstats)
library(dplyr)
library(readxl)
library(WDI)
library(countrycode)

str(wb_cachelist, max.level=1)

?wbcache

wbindicators(lang = "en")

new_cache <- wb_cache()


#Search
#WDIsearch()

################################# Human Capital Index###########################



HCI_inds <- WDI(indicator = "HD.HCI.OVRL")

HCI_inds <- HCI_inds %>% 
  rename(HCI= HD.HCI.OVRL)

HCI_inds$iso3c <- NULL 
HCI_inds$country <- NULL

HCI_inds_df <- HCI_inds %>%
  filter(year %in% c("2020")) %>% 
  mutate(Scaled_HCI = HCI *100)

HCI_inds_df$HCI <- NULL

#HCI_inds_df <- na.omit(HCI_inds_df)



###########################################################GDP##################


GDP_inds <- wb_search("gdp")

#head(GDP_inds)


gdp_fixed <- wb_data(indicator = "NY.GDP.MKTP.PP.CD")

#Filter out the dates
filtered_gdp_fixed <- gdp_fixed %>% 
  filter(date %in% c(2016, 2020, 2021, 2022))

filtered_gdp_fixed <- filtered_gdp_fixed %>%
  select(-iso3c,-country,-unit,-obs_status,-footnote,-last_updated,)

  
GDP_2016 <- filtered_gdp_fixed %>% 
  filter(date%in% c(2016))

GDP_2020 <- filtered_gdp_fixed %>% 
  filter(date%in% c(2020))

GDP_2021 <- filtered_gdp_fixed %>% 
  filter(date%in% c(2021))

GDP_2022 <- filtered_gdp_fixed %>% 
  filter(date%in% c(2022))
  

#GDP_2016 <- na.omit(GDP_2016)
#GDP_2020 <- na.omit(GDP_2020)
#GDP_2021 <- na.omit(GDP_2021)
#GDP_2022 <- na.omit(GDP_2022)


#######################################################Military expenditure##############################################


# Specify the Excel file path
Control_variables <- read_excel("~/Stockholms Universitet/Statsvet Kandidiat/Statistik analys/UN_Negotiations/CCW/Control variables.xlsx")


####################2016#########################################################

Mil_exp_2016 <- Control_variables[c("Country","2016")]

# Rename the column
Mil_exp_2016 <- Mil_exp_2016 %>%
  rename(Military = '2016')
Mil_exp_2016_clean <- Mil_exp_2016 %>%
  filter(Military != "...", Military != "xxx") 

# Replace non-numeric characters with NA and convert to numeric
Mil_exp_2016_clean$Military <- as.numeric(gsub("[^0-9\\.-]", "", Mil_exp_2016_clean$Military))

# Now that the Military column is numeric, you can round the values to 2 decimal places
Mil_exp_2016_clean$Military <- round(Mil_exp_2016_clean$Military, 2)

####################2020#########################################################

Mil_exp_2020 <- Control_variables[c("Country","2020")]

# Rename the column
Mil_exp_2020 <- Mil_exp_2020 %>%
  rename(Military = '2020')
Mil_exp_2020_clean <- Mil_exp_2020 %>%
  filter(Military != "...", Military != "xxx") 

# Replace non-numeric characters with NA and convert to numeric
Mil_exp_2020_clean$Military <- as.numeric(gsub("[^0-9\\.-]", "", Mil_exp_2020_clean$Military))

# Now that the Military column is numeric, you can round the values to 2 decimal places
Mil_exp_2020_clean$Military <- round(Mil_exp_2020_clean$Military, 2)


####################2021#########################################################

Mil_exp_2021 <- Control_variables[c("Country","2021")]

# Rename the column
Mil_exp_2021 <- Mil_exp_2021 %>%
  rename(Military = '2021')
Mil_exp_2021_clean <- Mil_exp_2021 %>%
  filter(Military != "...", Military != "xxx") 

# Replace non-numeric characters with NA and convert to numeric
Mil_exp_2021_clean$Military <- as.numeric(gsub("[^0-9\\.-]", "", Mil_exp_2021_clean$Military))

# Now that the Military column is numeric, you can round the values to 2 decimal places
Mil_exp_2021_clean$Military <- round(Mil_exp_2021_clean$Military, 2)

####################2022#########################################################

Mil_exp_2022 <- Control_variables[c("Country","2022")]

# Rename the column
Mil_exp_2022 <- Mil_exp_2022 %>%
  rename(Military = "2022")
Mil_exp_2022_clean <- Mil_exp_2022 %>%
  filter(Military != "...", Military != "xxx") 

# Replace non-numeric characters with NA and convert to numeric
Mil_exp_2022_clean$Military <- as.numeric(gsub("[^0-9\\.-]", "", Mil_exp_2022_clean$Military))

# Now that the Military column is numeric, you can round the values to 2 decimal places
Mil_exp_2022_clean$Military <- round(Mil_exp_2022_clean$Military, 2)



############################ JOINING TABLES FOR MULTIVARIAT REGRESSION**************************

#HCI_inds_df is the Human capital index


#Mil_exp_year_clean is the military expenditure

#GDP_Year are the gdps






#############2016##############


Mil_exp_2016_clean$iso2c <- countrycode(Mil_exp_2016_clean$Country, "country.name", "iso2c")


joined_multivariate_2016 <- left_join(Mil_exp_2016_clean,GDP_2016)
joined_multivariate_2016 <- left_join(joined_multivariate_2016,HCI_inds_df)

joined_multivariate_2016 <- joined_multivariate_2016 %>%
  select(-Country,-year,-date)

#joined_multivariate_2016 <- na.omit(joined_multivariate_2016)

########2020#################
Mil_exp_2020_clean$iso2c <- countrycode(Mil_exp_2020_clean$Country, "country.name", "iso2c")


joined_multivariate_2020 <- left_join(Mil_exp_2020_clean,GDP_2020)
joined_multivariate_2020 <- left_join(joined_multivariate_2020,HCI_inds_df)

joined_multivariate_2020 <- joined_multivariate_2020 %>%
  select(-Country,-year,-date)

#joined_multivariate_2020 <- na.omit(joined_multivariate_2020)

########2021##################

Mil_exp_2021_clean$iso2c <- countrycode(Mil_exp_2021_clean$Country, "country.name", "iso2c")


joined_multivariate_2021 <- left_join(Mil_exp_2021_clean,GDP_2021)
joined_multivariate_2021 <- left_join(joined_multivariate_2021,HCI_inds_df)

joined_multivariate_2021 <- joined_multivariate_2021 %>%
  select(-Country,-year,-date)

#joined_multivariate_2021 <- na.omit(joined_multivariate_2021)

########2022#########



Mil_exp_2022_clean$iso2c <- countrycode(Mil_exp_2022_clean$Country, "country.name", "iso2c")


joined_multivariate_2022 <- left_join(Mil_exp_2022_clean,GDP_2022)
joined_multivariate_2022 <- left_join(joined_multivariate_2022,HCI_inds_df)

joined_multivariate_2022 <- joined_multivariate_2022 %>%
  select(-Country,-year,-date)

#joined_multivariate_2022 <- na.omit(joined_multivariate_2022)
