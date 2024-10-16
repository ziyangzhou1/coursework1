
### MULTIPLE LINEAR REGRESSION ----------------------------------------------

#---- Preparatory work ----

#Loading the packages
library(tidyverse)

# ---- 1. Working with a dataset ----

#Importing data
library(readxl)

hotel <- read_excel("data/HotelCustomersDataset.xlsx")
glimpse(hotel)

#Printing 10 rows,4 cols
hotel[1:10, 1:4]

# Removing the columns containing the variables NameHash and DocIDHash from the dataset
hotel <- hotel |> 
  select(
    -c(contains("NameHash"), #contains() used for pattern-matching
    contains("DocIDHash"))
    )
glimpse(hotel)

## Calculating quests in the dataset
hotel$Age <- as.numeric(hotel$Age) #storing "Age" as numeric for successful cleaning

guests <- hotel |> 
  filter(Age > 0 & Age < 100, DaysSinceLastStay > 0, DaysSinceFirstStay > 0) |> # cleaning vars used for sorting
  distinct(DaysSinceLastStay, DaysSinceFirstStay, Nationality, Age)

paste("The number of unique guests approximated by variables 'DaysSinceLastStay', 'DaysSinceFirstStay', 'Nationality', 'Age' is", 
      nrow(guests))

#Removing negative values for 'DaysSinceFirstStay'
hotel <- hotel |> 
  filter(DaysSinceFirstStay >= 0)

any(hotel$DaysSinceFirstStay < 0)

#Transforming some columns to numeric
col_names <-  c("Age", "AverageLeadTime", "DaysSinceLastStay", "DaysSinceFirstStay", "LodgingRevenue", "OtherRevenue")

hotel[,col_names] <- as.data.frame(lapply(hotel[,col_names], function(x) as.numeric(x)))
glimpse(hotel)
                                   
# ---- 2. Creating plots ----

#Importing ggplot2
library(ggplot2)

#LodgingRevenue vs OtherRevenue
ggplot(data = hotel, mapping = aes(x = LodgingRevenue, y = OtherRevenue)) + 
  geom_point() + 
  labs(
    title = "Relationship between 'LodgingRevenue' and 'OtherRevenue'",
    )
ggsave("output/Relationship between 'LodgingRevenue' and 'OtherRevenue'.jpg")

#Age vs OtherRevenue 
ggplot(data = hotel, mapping = aes(x = Age, y = OtherRevenue)) + 
  geom_point() +
  labs(
    title = "Relationship between 'Age' and 'OtherRevenue'",
  )
ggsave("output/Relationship between 'Age' and 'OtherRevenue'.jpg")

#PersonsNights vs OtherRevenue 
ggplot(data = hotel, mapping = aes(x = PersonsNights, y = OtherRevenue)) + 
  geom_point() +  
  labs(
    title = "Relationship between 'PersonsNights' and 'OtherRevenue'",
  )
ggsave("output/Relationship between 'PersonsNights' and 'OtherRevenue'.jpg")

## Creating ranking of guest nationalities
top_nation <- guests |> 
  count(Nationality) |> 
  arrange(desc(n)) |> 
  slice_head(n = 20) |> 
  rename("number" = "n")

glimpse(top_nation)

# Creating a bar chart
ggplot(top_nation, aes(Nationality, number)) +
  geom_bar(stat = "identity", fill = "darkgrey", color = "black") + 
  geom_text(aes(label = number), vjust = - 0.5) +
  labs(
    title = "Ranking of top 20 guests' nationalities",
    y = "Number of guests"
  )

#Deleting the rows with NA values
hotel_no_na <- hotel |> 
  drop_na()

paste("There are", nrow(hotel_no_na), "rows left after deleting NA values.","(",nrow(hotel)-nrow(hotel_no_na), "were deleted.)") 

# ---- 3. Working with regressions ----

## Model 1

#Constructing a correlation matrix to see which variables are correlated with "OtherRevenue"
cor(
  hotel_no_na$OtherRevenue,
  hotel_no_na |> 
    select(-c("Nationality", "DistributionChannel", "MarketSegment", "ID"))
  )

model1 <- lm(
  OtherRevenue ~ LodgingRevenue + AverageLeadTime + PersonsNights + Age,
  data = hotel
  )
summary(model1)

#Interpreting the coefficient in report

## Model 2

#Adding 5 nationality dummy variables

hotel_nat_dummy <- hotel |> 
  mutate(
    value_one = 1
  ) |> 
  pivot_wider(
    names_from = Nationality,
    values_from = value_one,
    values_fill = list(value_one = 0)
  )
glimpse(hotel_nat_dummy)

hotel <- hotel |> 
  mutate(
    PRT = hotel_nat_dummy$PRT,
    GBR = hotel_nat_dummy$GBR,
    FRA = hotel_nat_dummy$FRA, 
    DEU = hotel_nat_dummy$DEU, 
    ESP = hotel_nat_dummy$ESP
  )
glimpse(hotel)

## Model 3

#Model2 with 5 dummies
model1_nat_dummy <- lm(
  OtherRevenue ~ LodgingRevenue + AverageLeadTime + PersonsNights + Age +
  PRT + GBR + FRA + DEU + ESP,
  data = hotel
  )
summary(model1_nat_dummy)

#Model3 with additional vars
model3 <- lm(
  OtherRevenue ~ LodgingRevenue + AverageLeadTime + PersonsNights + Age +
  PRT + GBR + FRA + DEU + ESP + SRLowFloor +  SRHighFloor + 
  SRAwayFromElevator + SRQuietRoom,
  data = hotel
)
summary(model3)

#Calculating the AIC and BIC for the models

aic_bic_df <- data.frame(
  models = c("MLR 1", "MLR 2 (with 5 dummy nationalities)", "MLR 3 (with some additional vars)"),
  AIC = c(AIC(model1), AIC(model1_nat_dummy), AIC(model3)),
  BIC = c(BIC(model1), BIC(model1_nat_dummy), BIC(model3))
)
print(aic_bic_df)
write_csv(aic_bic_df, "output/aic_bic_comparison.csv")

# ---- 4. Creating test dataset and making predictions ----

hotel <- hotel |> 
  drop_na()
write_csv(hotel, "output/clean_hotel.csv")

#Test dataset
set.seed(123)

test_indices <- sample(nrow(hotel), 10000)

test_hotel <- hotel[test_indices,]
train_hotel <- hotel[-test_indices,]
write_csv(test_hotel, "output/test_hotel.csv")
write_csv(train_hotel, "output/train_hotel.csv")

## Reestimating the models on the training data

# Model 1
model1_train <- lm(
  OtherRevenue ~ LodgingRevenue + AverageLeadTime + PersonsNights + Age,
  data = train_hotel
)
summary(model1)

# Model 2
model2_train <- lm(
  OtherRevenue ~ LodgingRevenue + AverageLeadTime + PersonsNights + Age +
    PRT + GBR + FRA + DEU + ESP,
  data = train_hotel
)
summary(model2_train)

# Model3
model3_train <- lm(
  OtherRevenue ~ LodgingRevenue + AverageLeadTime + PersonsNights + Age +
    PRT + GBR + FRA + DEU + ESP + SRLowFloor +  SRHighFloor + 
    SRAwayFromElevator + SRQuietRoom,
  data = train_hotel
)
summary(model3_train)

## Predicting the "OtherReturns"

#Model1
predictions_model1 <- predict(model1_train, test_hotel)
predictions_model1

#Model2
predictions_model2 <- predict(model2_train, test_hotel)
predictions_model2 

#Model3
predictions_model3 <- predict(model3_train, test_hotel)
predictions_model3 

test_hotel <- test_hotel |> 
  mutate(
    OtherRevenue_model1 = predictions_model1,
    OtherRevenue_model2 = predictions_model2,
    OtherRevenue_model3 = predictions_model3
  ) |> 
  relocate(
    OtherRevenue, .before = OtherRevenue_model1
  )
glimpse(test_hotel)
write_csv(test_hotel, "output/test_hotel+pred.csv")

# Computing MSE 
mse_df <- data.frame(
  models = c(
    "MLR 1", 
    "MLR 2 (with 5 dummy nationalities)", 
    "MLR 3 (with some additional vars)"
    ),
  MSE = c(
    mean((test_hotel$OtherRevenue_model1 - test_hotel$OtherRevenue)^2), 
    mean((test_hotel$OtherRevenue_model2 - test_hotel$OtherRevenue)^2),
    mean((test_hotel$OtherRevenue_model3 - test_hotel$OtherRevenue)^2)
  )
)

mse_df
write_csv(mse_df, "output/mse_models.csv")
