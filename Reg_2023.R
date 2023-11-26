#Import files

# install.packages("dplyr")
# install.packages("countrycode")

library(ggplot2)
library(dplyr)
library(countrycode)



#################################################################DATACLEANING##################



### Step 1 Data cleaning #####



# Extracting country names from the file names
sentiment_scores2023$Country <- gsub("_.+$", "", sentiment_scores2023$file) # Remove everything after the underscore
sentiment_scores2023$Country <- gsub("\\d", "", sentiment_scores2023$Country) # Remove digits

# Mapping country names to country codes
sentiment_scores2023$Country <- countrycode(sentiment_scores2023$Country, "country.name", "iso2c")

# Renaming the column
sentiment_scores2023 <- rename(sentiment_scores2023, Country = file)

# Your dataframe now has the countries coded by ISO 2-letter country codes and the column renamed



###Correcting NA's

# First, make sure you have the correct dataframe name, which appears to be sentiment_scores2023.
# Then, identify the rows where the country code is NA.
na_rows <- which(is.na(sentiment_scores2023$Country))

# Print the corresponding file names for those rows to understand which country names are not being recognized.
na_files <- sentiment_scores2023$file[na_rows]
print(na_files)

# Manually replace the NAs with the correct country codes. For example:
sentiment_scores2023$Country[na_rows[na_files == "New_Zeeland.txt"]] <- "NZ"
sentiment_scores2023$Country[na_rows[na_files == "Group_of_52.txt"]] <- "G52"
sentiment_scores2023$Country[na_rows[na_files == "United_States.txt"]] <- "US"
sentiment_scores2023$Country[na_rows[na_files == "LUX_GS.txt"]] <- "LU"


# Replace 'New_Zealand.txt', 'Group_of_52.txt', and 'United_States.txt' with the actual names of the files that have NA in your dataframe.



###Loading the AI Index into this file from previous research.

# Renaming AI_Index to ISOCODE:

AI_2023 <- AI_index
AI_2023$CountryCode = countrycode(AI_2023$Country, "country.name", "iso2c")



####Step 2 joining the tables of AI index and Sentiment Score####



#Renaming "Country" in sentiment score to "CountryCode"

# Using names()
names(sentiment_scores2023)[names(sentiment_scores2023) == "Country"] <- "CountryCode"

# This performs an inner join by default
joined_sentiment_scores2023 <- right_join(AI_2023,sentiment_scores2023,by = "CountryCode")




### Step3, Unbundling the Group of 52#####

# Define the vector of 'G52' full country names
g52_countries <- c("Argentina", "Australia", "Austria", "Belgium", "Bosnia and Herzegovina", 
                   "Bulgaria", "Canada", "Colombia", "Costa Rica", "Croatia", "Cyprus", 
                   "Czechia", "Denmark", "Ecuador", "Estonia", "Finland", "France", "Greece", 
                   "Guatemala", "Hungary", "Ireland", "Italy", "Japan", "Kazakhstan", 
                   "Republic of Korea", "Luxembourg", "Latvia", "Lithuania", "Republic of North Macedonia", 
                   "Malta", "Mexico", "Montenegro", "Netherlands", "New Zealand", "Norway", 
                   "Panama", "Peru", "Philippines", "Poland", "Portugal", "Romania", "Serbia", 
                   "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", 
                   "United Kingdom", "United States of America", "Uruguay", "Germany", "European Union")

# Convert the country names to ISO country codes
g52_country_codes <- countrycode(g52_countries, "country.name", "iso2c")

# Create a dataframe with these codes
g52_df <- data.frame(CountryCode = g52_country_codes)

g52_df$score <- 42.80

g52_AI_Score <- left_join(g52_df,AI_2023)
g52_AI_Score <- select(g52_AI_Score, -Country)


#Removing the ones who made individual statements

# Assuming your dataframe is named df and the country codes are in a column named "CountryCode"

# Define the list of country codes to be dropped
codes_to_drop <- c("FR", "IL", "PK", "CH", "NZ", "LU", "DE", "AT", "NL", "JP", "US")

# Exclude rows with the specified country codes
g52_AI_Score <- g52_AI_Score[!g52_AI_Score$CountryCode %in% codes_to_drop, ]

# Now df will only contain rows that do not have the specified country codes


#Now give randomized AI_Score to NA between 8.9 and 20


# Identify the NA values in AIScore
na_indices <- is.na(g52_AI_Score$AIScore)

# Generate random numbers for each NA value in the range of 8.9 to 20
g52_AI_Score$AIScore[na_indices] <- runif(sum(na_indices), min = 8.9, max = 20)
g52_AI_Score <- na.omit(g52_AI_Score)


joined_sentiment_scores2023_wg52 <- full_join(joined_sentiment_scores2023,g52_AI_Score)


#Remove G52

joined_sentiment_scores2023_wg52 <- joined_sentiment_scores2023_wg52[joined_sentiment_scores2023_wg52$CountryCode != 'G52', ]


# Remove the 'file' column
joined_sentiment_scores2023_wg52 <- select(joined_sentiment_scores2023_wg52, -file)

#remove country
# Remove the 'file' column
joined_sentiment_scores2023_wg52 <- select(joined_sentiment_scores2023_wg52, -Country)

###########################################################################################################




#Regression


#model <- lm(y ~ x, data=data)

Reg_2023 <- lm(score ~ AIScore, data = joined_sentiment_scores2023_wg52)

summary(Reg_2023)



#Scatterplot
ggplot(joined_sentiment_scores2023_wg52, aes(x=AIScore, y=score)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title="Governmental 2023", x="AI Score", y="Sentiment")















