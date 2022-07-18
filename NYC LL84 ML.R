##### SECTION 1: Introduction / Overview -------------------------------------------------

# Data set from NYC OpenData, can be found at 
# https://data.cityofnewyork.us/Environment/Energy-and-Water-Data-Disclosure-for-Local-Law-84-/usc3-8zwd

# Data described as Data and metrics on water and energy consumption in privately owned 
# buildings over 25,000 ft2 and in City-owned buildings over 10,000 ft2.

# Original data set has 28.1k rows and 250 columns


##### SECTION 2: Setup -------------------------------------------------------------------

# Load required packages
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(caret)) install.packages('caret')

library(tidyverse)
library(caret)


NYC_LL84 <- read.csv("Data/NYC_LL84_Clean.csv") 

# Create our test and train data for machine learning from reporting years 2016-18
NYC_LL84_ML <- NYC_LL84 %>%
  filter(year %in% c("2016", "2017", "2018")) %>%
  select(year, primary_property_type, year_built,
         number_of_buildings, energy_star_score, property_gfa, parking_gfa,
         leed_project, CDD, HDD, MeanMaxTemp, MeanMinTemp, site_eui)

# Create the validation set to be used in the very end 
NYC_LL84_Validation <- NYC_LL84 %>%
  filter(year == "2019") %>%
  select(year, primary_property_type, year_built,
         number_of_buildings, energy_star_score, property_gfa, parking_gfa,
         leed_project, CDD, HDD, MeanMaxTemp, MeanMinTemp, site_eui)



##### SECTION 3: Methods / Analysis ------------------------------------------------------

###### SECTION 3.1: Exploratory Analysis -------------------------------------------------


# Summary of data frame
glimpse(NYC_LL84_ML)

summary(NYC_LL84_ML$site_eui)

# EUI and GHG by Property Type
NYC_LL84_ML %>%
  group_by("Property Type" = primary_property_type) %>%
  summarise("Site EUI (kBtu/sqft)" = round(mean(site_eui), 
                                           digits = 1), 
            Count = n())  %>%
  arrange(desc(Count)) %>%
  knitr::kable()

# Explore EUI 
summary(NYC_LL84_ML$site_eui)

NYC_LL84_ML %>%
  mutate(primary_property_type = fct_reorder(primary_property_type, 
                                             site_eui)) %>%
  ggplot(aes(x = primary_property_type, 
             y = site_eui, 
             fill = primary_property_type)) +
  geom_boxplot() +
  labs(x = "Primary Property Type",
       y = "Site Energy Use Intensity (kBtu/sqft)",
       title = "Site EUI by Primary Property Type") +
  theme(legend.position = "none") +
  coord_flip()




# Size of building versus EUI
summary(NYC_LL84_ML$property_gfa)

NYC_LL84_ML %>%
  mutate(size_quartile = case_when(
    `property_gfa` > 113686 ~ 4,
    `property_gfa` > 63968 & `property_gfa` <= 113686 ~ 3,
    `property_gfa` > 40760 & `property_gfa` <= 63968 ~ 2,
    `property_gfa` <= 40760 ~ 1)) %>%
  group_by(size_quartile) %>%
  summarise("Site EUI (kBtu/sqft)" = round(mean(site_eui), 
                                           digits = 1),
            Count = n())  %>%
  arrange(size_quartile) %>%
  knitr::kable()
  

# Look at correlation by feature for total GHG emissions 
# Examine EUI data and check for outliers\

NYC_LL84_ML %>%
  select_if(., is.numeric) %>%
  cor() %>%
  as.data.frame() %>%
  rownames_to_column(var = "Feature") %>%
  select(Feature, site_eui) %>%
  drop_na() %>%
  arrange(desc(abs(site_eui))) %>%
  knitr::kable() 


###### SECTION 3.2: Actual Methods -------------------------------------------------------

# Partition the data in to a test set with 20% and train set with 80%
# Set seed to 92 for reproducing results  
set.seed(92, sample.kind = "Rounding")
test_index <- createDataPartition(NYC_LL84_ML$site_eui, 
                                  times = 1, p = 0.2, list = FALSE)
test_set <- NYC_LL84_ML %>% slice(test_index)
train_set <- NYC_LL84_ML %>% slice(-test_index)


### Method #1: Naive Model
# Start off with a naive model that uses the average rating to predict movie ratings
mu_hat <- mean(train_set$site_eui)
rmse_naive <- RMSE(pred = mu_hat,
                   obs = test_set$site_eui)


### Method #2: Using average EUI by property type 
rmse_eui_avg <- RMSE(left_join(test_set, 
                               train_set %>%
                                 group_by(primary_property_type) %>%
                                 summarize(pred_avg_eui = mean(site_eui)),
                               by = "primary_property_type") %>%
                       .$pred_avg_eui, 
                     obs = test_set$site_eui, 
                     na.rm = TRUE)


### Method #3: Linear Regression with Energy Star Score
# Create a linear model
set.seed(92, sample.kind = "Rounding")
model_lr <- train(site_eui ~ energy_star_score, 
               data = train_set, 
               method = "lm")

model_lr

# Generate predicted Site EUI using the test set
pred_lr <- predict(model_lr, 
                newdata = test_set %>%
                  select(site_eui, energy_star_score))

# Save the model RMSE
rmse_lr <- RMSE(pred = pred_lr, 
                    obs = test_set$site_eui, 
                    na.rm = TRUE)


### Method #4: Multiple Linear Regression with all variables
# Create a linear model
set.seed(92, sample.kind = "Rounding")
model_mlr <- train(site_eui ~ ., 
                   trControl = trainControl(method = "repeatedcv", repeats = 3),
                   data = train_set, 
                   method = "lm")

model_mlr

# Generate predicted Site EUI using the test set
pred_mlr <- predict(model_mlr, 
                    newdata = test_set)

# Save the model RMSE
rmse_mlr <- RMSE(pred = pred_mlr, 
                    obs = test_set$site_eui, 
                    na.rm = TRUE)

mae_mlr <- MAE(pred = pred_mlr, 
               obs = test_set$site_eui, 
               na.rm = TRUE)


### Method #5: Classification and Regression Tree (CART) with 'rpart'
# Need to add CART to packages list
set.seed(92, sample.kind = "Rounding")
model_rpart <- train(site_eui ~ ., 
                     data = train_set, 
                     trControl = trainControl(method = "repeatedcv", repeats = 3),
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(.00001, .001, .0001)))

# Generate predicted Site EUI using the test set
pred_rpart <- predict(model_rpart, 
                      newdata = test_set)

# Save the model RMSE
rmse_rpart <- RMSE(pred = pred_rpart, 
                   obs = test_set$site_eui, 
                   na.rm = TRUE)

ggplot(model_rpart, highlight = TRUE) +
  geom_text(position = position_nudge(x = 0.0001), 
            aes(label = ifelse(cp == model_rpart$bestTune[[1]], 
                               round(min(model_rpart$results$RMSE), digits = 2), ""))) +
  labs(title = "Optimal Complexity Paramter for CART Model") 

###### Validation 
# Generate predicted Site EUI using the test set
pred_validation <- predict(model_rpart, 
                           newdata = NYC_LL84_Validation)

# Save the model RMSE
rmse_validation <- RMSE(pred = pred_validation, 
                        obs = NYC_LL84_Validation$site_eui, 
                        na.rm = TRUE)


##### SECTION 4: Results -----------------------------------------------------------------

# This section presents the modeling results and discusses the model performance
# Final results table with test on validation set
results_tbl <- tibble(
  Method = c("Method #1", "Method #2", "Method #3", "Method #4", "Method #5"),
  Model = c("Naive Model", "EUI Averages", "LR - Energy Star", "Mulitple Linear Regression", "CART"),
  `Model Rsquared` = c("-", 
                       "-",
                       round(model_lr$results$Rsquared, digits = 4),
                       round(model_mlr$results$Rsquared, digits = 4),
                       round(max(model_rpart$results$Rsquared), digits = 4)),
  RMSE = c(rmse_naive, rmse_eui_avg, rmse_lr, rmse_mlr, rmse_rpart)) %>%
  mutate(`RMSE Improvment` = scales::percent((RMSE-rmse_naive)/rmse_naive))

results_tbl %>%
  mutate(RMSE = round(RMSE, digits = 2)) %>%
  knitr::kable()

Results <- cbind.data.frame(Actual = test_set$site_eui, 
                 `Linear Regression` = pred_lr, 
                 `Mulitple Linear Regression` = pred_mlr, 
                 `Knn` = pred_knn, 
                 `CART` = pred_rpart) %>%
  pivot_longer(cols = 2:5,
               values_to = "Predicted",
               names_to = "Model") %>%
  mutate(Residual = Actual-Predicted) %>%
  select(Model, Actual, Predicted, Residual)


# Actual vs predicted by model without outliers
Results %>%
  filter(Actual < 2500, Predicted < 2500) %>%
  ggplot(aes(x = Actual, y = Predicted, color = abs(Residual))) +
  geom_point(alpha = 0.8) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_smooth(color = "black") +
  coord_equal() +
  facet_wrap(~Model) +
  labs(color = "Residual",
       title = "Actual vs Predicted Values by Model",
       subtitle = "[For Non-Outlier Values]")








