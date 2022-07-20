##### SECTION 1: Introduction / Overview -------------------------------------------------

# The data set used for this machine learning exercise combines building information data and weather data for New York City. The building data was retrieved from NYC Open Data and is part of the city's Local Law 84 (LL84). LL84 is contributes to the city's building performance standard benchmarking efforts and applies to any privately owned buildings over 25,000 square feet and municipal buildings over 10,000 square feet. Most importantly, the data set includes metrics on energy use and consumption by building during a reporting year. This data was supplemented with weather data from 3 local weather stations - Central Park, LaGuardia, and Kennedy. The weather data was aggregated up to a calendar year and used as an average of all three stations.

# The project goal was to predict a building's Site Energy Use Intensity (EUI) using common and publicly available variables. Site EUI is metric commonly used for describing a buildings energy efficiency, typically with a unit of kBtu per square foot. Sites with lower EUI, use less energy per square foot. This can be particularly useful for benchmarking across similar property types, setting energy efficiency reduction targets, or assessing the impacts of energy efficiency improvements.

# Note: Please refer to Rmd or PDF versions for full descriptions

##### SECTION 2: Setup -------------------------------------------------------------------

# Load required packages
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(caret)) install.packages('caret')
if (!require(rpart)) install.packages('rpart')
if (!require(randomForest)) install.packages('randomForest')
if (!require(scales)) install.packages('scales')
if (!require(knitr)) install.packages('knitr')
if (!require(kableExtra)) install.packages('kableExtra')
if (!require(ggExtra)) install.packages('ggExtra')


library(tidyverse)
library(caret)
library(rpart)
library(randomForest)

# Load the cleaned up data for reporting years 2016 through 2019
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

# Summary stats for the Site EUI column
summary(NYC_LL84_ML$site_eui)

# View EUI by property type and frequency of occurrence
NYC_LL84_ML %>%
  group_by("Property Type" = primary_property_type) %>%
  summarise("Site EUI (kBtu/sqft)" = round(mean(site_eui), 
                                           digits = 1), 
            Count = n())  %>%
  arrange(desc(Count)) %>%
  knitr::kable()

# Create boxplot of site EUI by property type
NYC_LL84_ML %>%
  mutate(primary_property_type = fct_reorder(primary_property_type, 
                                             site_eui)) %>%
  ggplot(aes(x = primary_property_type, 
             y = site_eui, 
             fill = primary_property_type)) +
  geom_boxplot(size = .3, outlier.size = 0.3) +
  labs(x = "Primary Property Type",
       y = "Site Energy Use Intensity (kBtu/sqft)",
       title = "Site EUI by Primary Property Type") +
  theme(legend.position = "none") +
  coord_flip()

# Size of building versus EUI
summary(NYC_LL84_ML$property_gfa)

# Examine average EUI by building area quartile
NYC_LL84_ML %>%
  mutate(size_quartile = case_when(
    `property_gfa` > 135575 ~ 4,
    `property_gfa` > 80000 & `property_gfa` <= 135575 ~ 3,
    `property_gfa` > 53900 & `property_gfa` <= 80000 ~ 2,
    `property_gfa` <= 53900 ~ 1)) %>%
  group_by(size_quartile) %>%
  summarise("Site EUI (kBtu/sqft)" = round(mean(site_eui), 
                                           digits = 1),
            Count = n())  %>%
  arrange(size_quartile) %>%
  knitr::kable()

# Look at correlation by predictor for site EUI
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
# Generate predicted Site EUI values using the train set mean EUI
mu_hat <- mean(train_set$site_eui)


### Method #2: Using average EUI by property type 
# Generate predicted Site EUI using the test set predictors and property type averages
pred_eui_avg <- left_join(test_set, 
                          train_set %>%
                            group_by(primary_property_type) %>%
                            summarize(pred_avg_eui = mean(site_eui)),
                          by = "primary_property_type") %>%
  .$pred_avg_eui


### Method #3: Linear Regression with Energy Star Score
# Train a linear model using only ENERGY STAR score
set.seed(92, sample.kind = "Rounding")
model_lr <- train(site_eui ~ energy_star_score, 
                  data = train_set, 
                  method = "lm",
                  trControl = trainControl(method = "cv", number = 10))

# Generate predicted Site EUI using the test set predictors and simple linear regression
pred_lr <- predict(model_lr, 
                   newdata = test_set %>%
                     select(site_eui, energy_star_score))


### Method #4: Multiple Linear Regression with all variables
# Train a linear model with all variables included
# Use 10 fold cross-validation for resampling
set.seed(92, sample.kind = "Rounding")
model_mlr <- train(site_eui ~ ., 
                   data = train_set, 
                   method = "lm",
                   trControl = trainControl(method = "cv", number = 10))

# Generate predicted Site EUI using the test set predictors and MLR model
pred_mlr <- predict(model_mlr, 
                    newdata = test_set)


### Method #5: Classification and Regression Tree (CART) with 'rpart'
# Train a model using a CART method from 'rpart' package
# Use 10 fold cross-validation for resampling
set.seed(92, sample.kind = "Rounding")
model_rpart <- train(site_eui ~ ., 
                     data = train_set, 
                     method = "rpart",
                     trControl = trainControl(method = "cv", number = 10),
                     tuneGrid = data.frame(cp = seq(.00001, .001, .0001)))

# Generate predicted Site EUI using the test set predictors and CART model
pred_rpart <- predict(model_rpart, 
                      newdata = test_set)

# Generate a CART model plot which highlights our optimal tuning parameter (cp)
ggplot(model_rpart, highlight = TRUE) +
  geom_text(position = position_nudge(x = 0.0001), 
            aes(label = ifelse(cp == model_rpart$bestTune[[1]], 
                               round(min(model_rpart$results$RMSE), digits = 2), ""))) +
  labs(title = "Optimal Complexity Paramter for CART Model") 


### Method #6: Random Forest
# Create a sample of the train set in order to speed up the tuning process
set.seed(92, sample.kind = "Rounding")
sample_index <- sample(nrow(train_set), 2000)
sample_set <- train_set %>% slice(sample_index)


# Begin with sample set and minimal load test (low ntree and 5-fold cv) for optimal mtry
set.seed(92, sample.kind = "Rounding")
model_rf_default <- train(site_eui ~ ., 
                          data = sample_set,
                          method = "rf", 
                          ntree = 100,
                          trControl = trainControl(method = "cv", number = 5),
                          tuneGrid = data.frame(mtry = 5:20))

# View optimal tuning parameter (mtry) in a plot
ggplot(model_rf_default, highlight = TRUE) +
  geom_text(position = position_nudge(y = 0.2), 
            aes(label = ifelse(mtry == model_rf_default$bestTune[[1]], 
                               round(min(model_rf_default$results$RMSE), 
                                     digits = 2), ""))) +
  labs(title = "Optimal Number of Randomly Selected Predictors for Random Forest",
       subtitle = "Using Sample Set (n = 2,000) and 100 Trees") 


# Find number of optimal trees to include in the model
ntrees <- c(100, 250, 500, 750, 1000)

# Use the sample test and optimal mtry to test out various tree sizes
tree_test <- sapply(ntrees, function(ntrees) {
  set.seed(92, sample.kind = "Rounding")
  model_rf <- train(site_eui ~ ., 
                    data = sample_set,
                    method = "rf", 
                    ntree = ntrees,
                    trControl = trainControl(method = "cv", number = 5),
                    tuneGrid = data.frame(mtry = model_rf_default$bestTune[[1]]))
  min(model_rf$results$RMSE)
})

tree_results <- data.frame("ntrees" = ntrees, 
                           "rmse" = tree_test)  

# Create the actual random forest model using our tuning parameters above
# WARNING - Takes quite a long time to execute... 37.7 minutes
set.seed(92, sample.kind = "Rounding")
model_rf <- train(site_eui ~ ., 
                  data = train_set,
                  method = "rf", 
                  ntree = tree_results %>% slice_min(order_by = rmse) %>% .$ntrees,
                  tuneGrid = data.frame(mtry = model_rf_default$bestTune[[1]]),
                  trControl = trainControl(method = "cv"))

# Generate predicted Site EUI using the test set predictors and random forest model
pred_rf <- predict(model_rf, 
                   newdata = test_set)

# Find variable importance for the random forest model
importance_df <- varImp(model_rf) 
importance_df <- importance_df[["importance"]] %>%
  rownames_to_column(var = "Feature") 

# Display top 10 most important variables in a bar plot
importance_df %>%
  mutate(Feature = gsub(pattern = "primary_property_type", 
                        replacement = "Type = ",
                        x = Feature),
         Feature = fct_reorder(Feature, Overall)) %>%
  slice_max(order_by = Overall, n = 10) %>%
  ggplot(aes(Feature, Overall)) +
  geom_col(fill = "lightblue", alpha = .9) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 25)) +
  coord_flip() +
  theme_light() +
  labs(title = "Feature Importance in the Random Forest Model",
       subtitle = "[Top 10 Features, Scaled]")


###### SECTION 3.3: Validation -----------------------------------------------------------
# Generate predicted Site EUI using the validation set and random forest model
pred_validation <- predict(model_rf, 
                           newdata = NYC_LL84_Validation)


##### SECTION 4: Results -----------------------------------------------------------------

###### SECTION 4.1: Comparing All Models -------------------------------------------------

# Save predicted values and residuals for all models in a data frame
Results <- cbind.data.frame(Actual = test_set$site_eui, 
                            `Naive Model` = mu_hat,
                            `Property Type Averages` = pred_eui_avg,
                            `Linear Regression` = pred_lr, 
                            `Multiple Linear Regression` = pred_mlr, 
                            `CART` = pred_rpart,
                            `Random Forest` = pred_rf) %>%
  pivot_longer(cols = 2:7,
               values_to = "Predicted",
               names_to = "Model") %>%
  mutate(Residual = Predicted - Actual) %>%
  select(Model, Actual, Predicted, Residual)

# Create model factor levels based on order of our methods (increasing complexity)
Results$Model = factor(Results$Model, levels = c("Naive Model", 
                                                 "Property Type Averages",
                                                 "Linear Regression", 
                                                 "Multiple Linear Regression",
                                                 "CART", 
                                                 "Random Forest"))

# Regression plots for actual vs predicted by model with color as RMSE 
Results %>%
  ggplot(aes(x = Actual, y = Predicted, color = abs(Residual))) +
  geom_point(alpha = 0.8, size = 1) +
  scale_color_gradient(low = "blue", high = "red") +
  geom_smooth(color = "black", size = .5) +
  ylim(0, 700) +
  xlim(0, 700) +
  coord_equal() +
  facet_wrap(~Model) +
  labs(x = "Actual EUI (kBtu/sqft)",
       y = "Predicted EUI (kBtu/sqft)",
       color = "Absolute\nResidual",
       title = "Regression Plots - Actual vs Predicted EUIs by Model") +
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot of residuals by model
Results %>%
  ggplot(aes(x = Model, y = Residual, color = Model)) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5)) +
  coord_flip()

# Set the knitr options to omit NA from table output
opts <- options(knitr.kable.NA = "")


# Creating a function that uses predicted values to calculate R-squared, MAE, and RMSE 
gen_results <- function(model) {
  results <- postResample(pred = model, obs = test_set$site_eui) %>% 
    as.data.frame() %>%
    t() 
}

# Final results table with method, model, R-squared, MAE, and RMSE 
Results_Summary <- tibble(
  Method = c("Method #1", "Method #2", "Method #3", "Method #4", 
             "Method #5", "Method #6", "Validation"),
  Model = c("Naive Model", "Property Type EUI Averages", "LR - ENERGY STAR Score", 
            "Mulitple Linear Regression", "CART", "Random Forest", "Random Forest"),
  rbind.data.frame(
    gen_results(mu_hat), gen_results(pred_eui_avg), gen_results(pred_lr),
    gen_results(pred_mlr), gen_results(pred_rpart), gen_results(pred_rf),
    postResample(pred = pred_validation, obs = NYC_LL84_Validation$site_eui) %>% 
      as.data.frame() %>% t())
) %>%
  mutate(`RMSE Improvement` = scales::percent((RMSE-RMSE[[1]])/RMSE[[1]])) %>%
  select(Method, Model, Rsquared, MAE, RMSE, `RMSE Improvement`) 

Results_Summary %>%
  knitr::kable(align = c('l', 'l', rep('c', 4)), 
               digits = 2,
               booktabs = TRUE,
               caption = "Final Summary Table for All Models") %>%
  kableExtra::row_spec(7, bold = TRUE) %>%
  kableExtra::row_spec(6, hline_after = TRUE)

###### SECTION 4.2: Validation Set Exploratory Analysis ----------------------------------

# Validation set - actual vs predicted with marginal distribution
ggExtra::ggMarginal(cbind.data.frame(Actual = NYC_LL84_Validation$site_eui, 
                                     `Predicted` = pred_validation) %>% 
                      mutate(Residual = Predicted - Actual) %>%
                      ggplot(aes(x = Actual, y = Predicted, color = abs(Residual))) +
                      geom_point(alpha = 0.8) +
                      scale_color_gradient(low = "blue", high = "red") +
                      geom_smooth(color = "black") +
                      ylim(0, 700) +
                      xlim(0, 700) +
                      coord_equal(ratio = 1) +
                      labs(x = "Actual EUI (kBtu/sqft)",
                           y = "Predicted EUI (kBtu/sqft)",
                           color = "Residual",
                           title = "Actual vs Predicted Values",
                           subtitle = "For Random Forest Model") +
                      theme_classic() +
                      theme(legend.position = "bottom",
                            plot.title = element_text(hjust=0.5),
                            plot.subtitle = element_text(hjust=0.5)) + 
                      guides(color = guide_colourbar(ticks = FALSE, 
                                                     label = FALSE,
                                                     barheight = 0.8,
                                                     title.vjust = 0.9)),
                    type = "density", 
                    color = "darkgray")

# RMSE by property type for the validation set
cbind.data.frame(Actual = NYC_LL84_Validation$site_eui, 
                 `Predicted` = pred_validation,
                 `Property Type` = NYC_LL84_Validation$primary_property_type) %>% 
  mutate(Residual = Predicted - Actual) %>%
  group_by(`Property Type`) %>%
  summarize(RMSE = round(RMSE(Predicted, Actual, na.rm = TRUE), digits = 2),
            Count = n()) %>%
  arrange(desc(Count)) %>%
  knitr::kable()

##### SECTION 5: Conclusion --------------------------------------------------------------

#The goal of this project was to predict a building's Site EUI with machine learning algorithms. To find a final model with optimal performance, I tested out several methods with increasing complexity. They consist of a naive model, using property type EUI averages, simple linear regression, multiple linear regression, classification and regression tree, and random forest. **The random forest model had the best performance, with a** **46.4% RMSE improvement over the initial naive model.**

#This model can be used for many things including predicting energy use in future years with temperature changes, predicting site EUI for new properties or ones with missing energy consumption calculations, or flagging potential errors in user inputs. All applications which can assist New York City in achieving it's energy reduction targets.

# Note: Please refer to Rmd or PDF versions for full descriptions


