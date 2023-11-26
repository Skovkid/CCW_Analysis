#Import files

# install.packages("dplyr")
# install.packages("countrycode")

library(ggplot2)
library(dplyr)
library(countrycode)



#################################################################DATACLEANING##################



### Step 1 Data cleaning #####



# Extracting country names from the file names
sentiment_scores_2016$Country <- gsub("_.+$", "", sentiment_scores_2016$file) # Remove everything after the underscore
sentiment_scores_2016$Country <- gsub("\\d", "", sentiment_scores_2016$Country) # Remove digits

# Mapping country names to country codes
sentiment_scores_2016$Country <- countrycode(sentiment_scores_2016$Country, "country.name", "iso2c")

# Renaming the column
sentiment_scores_2016 <- rename(sentiment_scores_2016, Country = file)

# Your dataframe now has the countries coded by ISO 2-letter country codes and the column renamed



###Correcting NA's

# First, make sure you have the correct dataframe name, which appears to be sentiment_scores_2016.
# Then, identify the rows where the country code is NA.
na_rows <- which(is.na(sentiment_scores_2016$Country))

# Print the corresponding file names for those rows to understand which country names are not being recognized.
na_files <- sentiment_scores_2016$file[na_rows]
print(na_files)

# Manually replace the NAs with the correct country codes. For example:
sentiment_scores_2016$Country[na_rows[na_files == "EU.txt"]] <- "EU"




###Loading the AI Index into this file from previous research.##########################

# Renaming AI_Index to ISOCODE:

AI__2016 <- AI_index
AI__2016$CountryCode = countrycode(AI__2016$Country, "country.name", "iso2c")



####Step 2 joining the tables of AI index and Sentiment Score####



#Renaming "Country" in sentiment score to "CountryCode"

# Using names()
names(sentiment_scores_2016)[names(sentiment_scores_2016) == "Country"] <- "CountryCode"

# This performs an inner join by default
joined_sentiment_scores_2016 <- right_join(AI__2016,sentiment_scores_2016,by = "CountryCode")


#Ascribing EU a value of 50.0 in the AI index

joined_sentiment_scores_2016$AIScore[joined_sentiment_scores_2016$CountryCode == "EU"] <- 50.0



#Now give randomized AI_Score to NA between 8.9 and 20



# Identify the NA values in AIScore
na_indices <- is.na(joined_sentiment_scores_2016$AIScore)

# Generate random numbers for each NA value in the range of 8.9 to 20
joined_sentiment_scores_2016$AIScore[na_indices] <- runif(sum(na_indices), min = 8.9, max = 20)


# Remove the 'file' column
joined_sentiment_scores_2016 <- select(joined_sentiment_scores_2016, -file)

#remove country
# Remove the 'file' column
joined_sentiment_scores_2016 <- select(joined_sentiment_scores_2016, -Country)

###########################################################################################################




#Regression


#model <- lm(y ~ x, data=data)

Reg__2016 <- lm(score ~ AIScore, data = joined_sentiment_scores_2016)

summary(Reg__2016)



#Scatterplot
ggplot(joined_sentiment_scores_2016, aes(x=AIScore, y=score)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title="Governmental _2016", x="AI Score", y="Sentiment")















