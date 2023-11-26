#Import files

# install.packages("dplyr")
# install.packages("countrycode")

library(ggplot2)
library(dplyr)
library(countrycode)



#################################################################DATACLEANING##################



### Step 1 Data cleaning #####



# Extracting country names from the file names
sentiment_scores2020$Country <- gsub("_.+$", "", sentiment_scores2020$file) # Remove everything after the underscore
sentiment_scores2020$Country <- gsub("\\d", "", sentiment_scores2020$Country) # Remove digits

# Mapping country names to country codes
sentiment_scores2020$Country <- countrycode(sentiment_scores2020$Country, "country.name", "iso2c")

# Renaming the column
sentiment_scores2020 <- rename(sentiment_scores2020, Country = file)

# Your dataframe now has the countries coded by ISO 2-letter country codes and the column renamed



###Correcting NA's

# First, make sure you have the correct dataframe name, which appears to be sentiment_scores2020.
# Then, identify the rows where the country code is NA.
na_rows <- which(is.na(sentiment_scores2020$Country))

# Print the corresponding file names for those rows to understand which country names are not being recognized.
na_files <- sentiment_scores2020$file[na_rows]
print(na_files)

# Manually replace the NAs with the correct country codes. For example:
sentiment_scores2020$Country[na_rows[na_files == "United_States_1.txt"]] <- "US"
sentiment_scores2020$Country[na_rows[na_files == "United_States_2.txt"]] <- "US"
sentiment_scores2020$Country[na_rows[na_files == "United_States_3.txt"]] <- "US"
sentiment_scores2020$Country[na_rows[na_files == "Costa_Rica.txt"]] <- "CR"

#Removing unnecessary empty txt file
sentiment_scores2020 <- na.omit(sentiment_scores2020)



#Combining the scores

sentiment_scores2020<- sentiment_scores2020 %>%
  group_by(CountryCode) %>%
  summarise(total_score = sum(score, na.rm = TRUE))  # Sum scores, removing NA values



###Loading the AI Index into this file from previous research.

# Renaming AI_Index to ISOCODE:

AI_2020 <- AI_index
AI_2020$CountryCode = countrycode(AI_2020$Country, "country.name", "iso2c")



####Step 2 joining the tables of AI index and Sentiment Score####



#Renaming "Country" in sentiment score to "CountryCode"

# Using names()
names(sentiment_scores2020)[names(sentiment_scores2020) == "Country"] <- "CountryCode"

# This performs an inner join by default
joined_sentiment_scores2020 <- right_join(AI_2020,sentiment_scores2020,by = "CountryCode")


#Now give randomized AI_Score to NA between 8.9 and 20


# Identify the NA values in AIScore
na_indices <- is.na(joined_sentiment_scores2020$AIScore)

# Generate random numbers for each NA value in the range of 8.9 to 20
joined_sentiment_scores2020$AIScore[na_indices] <- runif(sum(na_indices), min = 8.9, max = 20)







# Remove the 'file' column
joined_sentiment_scores2020 <- select(joined_sentiment_scores2020, -file)

#remove country
# Remove the 'file' column
joined_sentiment_scores2020 <- select(joined_sentiment_scores2020, -Country)

###########################################################################################################




#Regression


#model <- lm(y ~ x, data=data)

Reg_2020 <- lm(total_score ~ AIScore, data = joined_sentiment_scores2020)

summary(Reg_2020)



#Scatterplot
ggplot(joined_sentiment_scores2020, aes(x=AIScore, y=total_score)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title="Governmental 2020", x="AI Score", y="Sentiment")















