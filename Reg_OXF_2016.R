#    REGRESSION FOR 2016 SENTIMENT SCORE      #


# FOR THIS REGRESSION, THE 2020 DATA WILL BE USED SINCE 2016 DATA IS NOT AVAILAIBLE
# 2017 INDEX HAD INSUFFICIENT N-COUNTRIES
library(ggplot2)
library(dplyr)
library(countrycode)
library(car)
library(ggrepel)  


setwd("C:/Users/Viktor/Documents/Stockholms Universitet/Statsvet Kandidiat/Statistik analys/UN_Negotiations/CCW/CCW_Analysis")

#Merge sentiment score with AI index

Oxf_sentiment_scores_2016 <- full_join(Oxf_AI_2020,sentiment_scores_2016_EU)

Oxf_sentiment_scores_2016 <- na.omit(Oxf_sentiment_scores_2016)



# Standard linear Regression


#model <- lm(y ~ x, data=data)

Oxf_reg_2016 <- lm(score ~ AI_Index, data = Oxf_sentiment_scores_2016)

summary(Oxf_reg_2016)

confint(Oxf_reg_2016)

Oxf_conf_intervals_2016 <- confint(Oxf_reg_2016)
view(Oxf_conf_intervals_2016)



#Scatterplot
ggplot(Oxf_sentiment_scores_2016, aes(x=AI_Index, y=score)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, color="red") +
  labs(title="CCW 5th Review (2016, Unbundled)", x="AI Score", y="Sentiment")








################## Control Variables


Oxf_mult_var_reg_2016 <-full_join(Oxf_sentiment_scores_2016,joined_multivariate_2016)
Oxf_mult_var_reg_2016 <- na.omit(Oxf_mult_var_reg_2016)

Oxf_mult_var_reg_2016_lm <- lm(score ~ AI_Index + + Military + NY.GDP.MKTP.PP.CD + Scaled_HCI, data = Oxf_mult_var_reg_2016)
summary(Oxf_mult_var_reg_2016_lm)

Oxf_Vif_2016 <- vif(Oxf_mult_var_reg_2016_lm)
print(Oxf_Vif_2016)



################### Colored graph


# Define the groups
g7_countries <- c("US", "CA", "JP", "GB", "FR", "DE", "IT")
red_countries <- c("CN", "RU", "BY", "KZ", "AM", "KG", "TJ", "IR")


# Define the groups
g7_countries <- c("US", "CA", "JP", "GB", "FR", "DE", "IT")
red_countries <- c("CN", "RU", "BY", "KZ", "AM", "KG", "TJ", "IR")

# Create new columns for color and size, and a label for the countries
Oxf_sentiment_scores_2016 <- Oxf_sentiment_scores_2016 %>%
  mutate(
    color = case_when(
      Country %in% g7_countries ~ "blue",
      Country %in% red_countries ~ "red",
      TRUE ~ "grey"
    ),
    size = case_when(
      Country %in% g7_countries ~ 5,
      Country %in% red_countries ~ 5,
      TRUE ~ 2
    ),
    label = if_else(Country %in% c(g7_countries, red_countries), as.character(Country), NA_character_)
  )


# Plot with color-blind friendly colors, different sizes, and labels for G7 and red countries
ggplot(Oxf_sentiment_scores_2016, aes(x = AI_Index, y = score)) +
  geom_point(aes(color = color, size = size)) +
  geom_text_repel(aes(label = label), na.rm = TRUE, size = 3) +  # Add labels with ggrepel for better placement
  geom_smooth(method = "lm", se = FALSE, color = "black") +  # Single regression line
  scale_color_manual(values = c("blue" = "blue", "red" = "red", "grey" = "grey")) +
  labs(title = "CCW 5th review Unbundled") +
  theme_minimal() +
  theme(legend.position = "none")  # Hide the legend for size
