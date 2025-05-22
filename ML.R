install.packages("naniar")
install.packages("simputation")
install.packages("mice")
install.packages("dplyr")
install.packages("pheatmap") 
install.packages("tidyr")
install.packages("glmnet")
library("glmnet")
library("pheatmap")
library("mice")
library("simputation")
library("naniar")
library("ggplot2")

library("dplyr")
library("tidyr")

datafifa <- read.csv("C:/Users/fotis/OneDrive/Υπολογιστής/SPORT DATASET/fifa_eda_stats.csv", na.strings = c("", "NA", "N/A"), sep = ",")  

set.seed(321)
str(datafifa)


## DATA- PRE-PROCESSING 
## DATA- EXPLORATION

# No duplicates
duplicated(datafifa) 

# Missing data between 3.2% and 7.8%, most missing variables in Release.Clause
Missingdata <- colSums(is.na(datafifa))/nrow(datafifa) * 100
Missingdata
gg_miss_var(datafifa)
vis_miss(datafifa)


## FEATURE ENGINEERING

#Binning variables
#Binning positions
datafifa$Position <- as.character(datafifa$Position)
datafifa <- datafifa %>%
  mutate(position_category = case_when(
    Position %in% c("GK") ~ "Goalkeeper",
    Position %in% c("CB", "RCB", "LCB", "LB", "RB", "LWB", "RWB") ~ "Defender",
    Position %in% c("CDM", "LDM", "RDM", "CM", "LCM", "RCM", "CAM", "LAM", "RAM") ~ "Midfielder",
    Position %in% c("LM", "RM", "LW", "RW", "ST", "CF", "LS", "RS") ~ "Attacker",
    is.na(Position) ~ NA_character_,
    TRUE ~ "Other"  # In case there are positions outside the categories
  ))
datafifa$position_category <- factor(datafifa$position_category, 
                                     levels = c("Goalkeeper", "Defender", "Midfielder", "Attacker"))

# Binning clubs in leagues
datafifa$Club[datafifa$Club == "FC Bayern M\xfcnchen"] <- "FC Bayern Munchen"
datafifa$Club[datafifa$Club == "Atl\xe9tico Madrid"] <- "Atletico Madrid"
datafifa$Club[datafifa$Club == "AS Saint-\xc9tienne"] <- "AS Saint-Etienne"
datafifa$Club[datafifa$Club == "Be?ikta? JK"] <- "Besiktas JK"
datafifa$Club[datafifa$Club == "Gr\xeamio"] <- "Gremio"
datafifa$Club[datafifa$Club == "Atl\xe9tico Mineiro"] <- "Atletico Mineiro"
datafifa$Club[datafifa$Club == "Borussia M\xf6nchengladbach"] <- "Borussia Monchengladbach"
datafifa$Club[datafifa$Club == "Medipol Başakşehir FK"] <- "Medipol Basaksehir FK"
datafifa$Club[datafifa$Club == "Fenerbah\xe7e SK"] <- "Fenerbahce SK"
datafifa$Club[datafifa$Club == "Deportivo Alav\xe9s"] <- "Deportivo Alaves"

datafifa <- datafifa %>%
  mutate(league = case_when(
    is.na(Club) ~ NA_character_,  # Keep NAs as NA
    Club %in% c("Manchester United", "Liverpool", "Chelsea", "Manchester City", "Arsenal", "Tottenham Hotspur", "Leicester City", "Everton", "West Ham United", "Wolverhampton Wanderers", "Crystal Palace", "Fulham", "Watford", "Burnley", "Stoke City", "Southampton") ~ "Premier League",
    Club %in% c("Real Madrid", "FC Barcelona", "Atletico Madrid", "Valencia CF", "Real Betis", "Real Sociedad", "Villarreal CF", "Sevilla FC", "Athletic Club de Bilbao", "Levante UD", "SD Eibar", "RC Celta", "Deportivo Alaves", "Girona FC") ~ "La Liga",
    Club %in% c("Juventus", "Milan", "Inter", "Napoli", "Lazio", "Roma", "Atalanta", "Torino", "Sassuolo", "Sampdoria") ~ "Serie A",
    Club %in% c("FC Bayern Munchen", "Borussia Dortmund", "RB Leipzig", "FC Schalke 04", "Bayer 04 Leverkusen", "RB Leipzig", "TSG 1899 Hoffenheim", "Borussia Monchengladbach", "Herta BSC", "SV Werder Bremen", "VfL Wolfsburg", "VfB Stuttgart", "Eintracht Frankfurt") ~ "Bundesliga",
    Club %in% c("Paris Saint-Germain", "Olympique Lyonnais", "Olympique de Marseille", "AS Saint-Etienne", "AS Monaco", "OGC Nice", "Montpellier HSC") ~ "Ligue 1",
    Club %in% c("FC Porto", "Besiktas JK", "Sporting CP", "SL Benfica", "Medipol Basaksehir FK", "Shakhtar Donetsk", "Galatasaray SK", "Fenerbahce SK", "SC Braga", "PSV", "Ajax") ~ "Other european",
    Club %in% c("Vissel Kobe", "Guangzhou Evergrande Taobao FC", "LA Galaxy", "Gremio", "Atletico Mineiro", "Dalian YiFang FC", "Shanghai SIPG FC", "Al Nassr", "Cruzeiro", "Beijing Sinobo Guoan FC", "PFC CSKA Moscow", "Toronto FC", "Lokomotiv Moscow", "New York City FC", "Fluminense", "Atlanta United", "Los Angeles FC", "Al Hilal", "River Plate", "Guangzhou R&F; FC") ~ "Other non-european", 
  ))

# Convert the league column to a factor with the desired levels
datafifa$league <- factor(datafifa$league, 
                          levels = c("Premier League", "La Liga", "Serie A", "Bundesliga", "Ligue 1", "Other european", "Other non-european"))   
# Binning age
datafifa <- datafifa %>%
  mutate(age_group = case_when(
    Age < 21 ~ "<21",
    Age >= 21 & Age <= 25 ~ "21-25",
    Age >= 26 & Age <= 30 ~ "26-30",
    Age > 30 ~ "30+",
    TRUE ~ NA_character_  # In case there are NA values in the age column
  ))

# Calculate years left on contract
datafifa <- datafifa %>%
  mutate(
    Contract.Valid.Until = as.numeric(Contract.Valid.Until),  
    contract_left = Contract.Valid.Until - 2019 
  )
datafifa$contract_left <- factor(datafifa$contract_left, ordered = TRUE)

# Create a new 'performance' variable based on position and other factors
datafifa <- datafifa %>%
  mutate(performance = case_when(
    position_category == "Goalkeeper" ~ (GKDiving + GKHandling + GKKicking + GKPositioning + GKReflexes) / 5,
    position_category == "Defender" ~ (Marking + StandingTackle + SlidingTackle + Interceptions + Aggression) / 5,  
    position_category == "Midfielder" ~ (ShortPassing + LongPassing + Vision + Stamina) / 4,
    position_category == "Attacker" ~ (Finishing + Crossing + Dribbling + BallControl) / 4,    
  ))


# IMPUTATION
# Identify all categorical columns (those with character type)
categorical_columns <- sapply(datafifa, is.character)

# Convert these categorical columns into factors
datafifa[categorical_columns] <- lapply(datafifa[categorical_columns], as.factor)

# Check for the number of missing values in each column
missing_values <- sapply(datafifa[, c("age_group", "contract_left", "Value", "International.Reputation", "league", "position_category", "Potential", "Overall")], function(x) sum(is.na(x)))
print(missing_values)

       
# Perform imputation using the specified methods for selected columns
datafifa$International.Reputation <- factor(datafifa$International.Reputation, 
                                            levels = 0:5, 
                                            ordered = TRUE)



# Impute missing values for selected columns
my_imp1 <- mice(datafifa[, c("age_group", "contract_left", "Value", "International.Reputation")], 
                m = 5, 
                method = c("polyreg", "polyreg", "pmm", "polyreg"),
                maxit = 50)

my_imp2 <- mice(datafifa[, c("league", "position_category", "Potential", "Wage", "performance")], 
                m = 5, 
                method = c("polyreg", "polyreg", "pmm", "pmm", "pmm"),
                maxit = 50)

       
  summary(datafifa$Value)
  my_imp1$imp$Value
       
  summary(datafifa$Potential)
  my_imp2$imp$Potential
       
       # Extract the imputed dataset (e.g., the 5th imputation)
       
       final_clean1 = complete(my_imp1,4)
       final_clean2 = complete(my_imp2,5)
       datafifa_clean <- cbind(final_clean1, final_clean2)
       
       sapply(datafifa_clean[, c("age_group", "contract_left", "Value", "International.Reputation", "league", "position_category", "Potential")], function(x) sum(is.na(x)))
       
       # Log transformation
       datafifa_log <- datafifa_clean %>%
         mutate(across(where(is.numeric), ~log(.)))
       
      

       # Make test- and training set
       test_set <- datafifa_log[seq(5, nrow(datafifa), by = 5),]
       training_set <- datafifa_log[-seq(5, nrow(datafifa), by = 5),]
       
       
       #### I TRIED THIS METHODS FOR IMPROVEMENT BUT THEY DIDNT WORK ####
       
       # # Handling outliers using IQR method on log-transformed data
       # Q1_train <- quantile(training_set$Value, 0.25)
       # Q3_train <- quantile(training_set$Value, 0.75)
       # IQR_train <- Q3_train - Q1_train 
       
       # # Apply the IQR method to remove outliers in training set
       # training_set_clean <- training_set[training_set$Value >= (Q1_train - 1.5 * IQR_train) & training_set$Value <= (Q3_train + 1.5 * IQR_train), ]
       # 
       # Q1_test <- quantile(test_set$Value, 0.25)
       # Q3_test <- quantile(test_set$Value, 0.75)
       # IQR_test <- Q3_test - Q1_test
       
       # # Apply the IQR method to remove outliers in test set
       # test_set_clean <- test_set[test_set$Value >= (Q1_test - 1.5 * IQR_test) & test_set$Value <= (Q3_test + 1.5 * IQR_test), ]
       
      
       ## Data exploration
       summary(datafifa$Value)
       summary(datafifa$Age)
       summary(datafifa$Contract.Valid.Until)
       
       summary(datafifa_clean$Value)
       
       
       # Outliers in value
       ggplot(datafifa, aes(x = "", y = Value/1000000)) +
         geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2) +
         labs(title = "Boxplot of value", y = "Value") +
         theme_minimal()
       
       # Outliers in age
       ggplot(datafifa, aes(x = "", y = Age)) +
         geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2) +
         labs(title = "Boxplot of age", y = "Value") +
         theme_minimal()
       
      
       
       ## DATA VISUALISATION
       
       # plot for age and value: decline in value after 30 years
       ggplot(datafifa, aes(x = Age, y = Value/1000000)) + geom_point()
       
       # Plot for age and potential: potential declines with age
       ggplot(datafifa, aes(x = Age, y = Potential)) + geom_point()
       
       # Correlation heatmap, not very useful?
       correlation_matrix <- cor(datafifa_clean[, sapply(datafifa_clean, is.numeric)], use = "complete.obs")
       pheatmap(correlation_matrix, 
                color = colorRampPalette(c("blue", "white", "red"))(100), 
                main = "Correlation Heatmap", 
                cluster_rows = TRUE, 
                cluster_cols = TRUE)
       
       # Violin plot
       ggplot(datafifa_clean, aes(x = age_group, y = Value/1000000)) +
         geom_violin(fill = "lightblue", color = "black") +   
         geom_boxplot(width = 0.1, fill = "white", color = "black") +  
         labs(title = "Market Value Across Different Age Groups", 
              x = "Age Group", 
              y = "Market Value") +
         theme_minimal() +
         theme(axis.text.x = element_text(angle = 45, hjust = 1))
       
       
    
       
       
       ## DATA MODELING / STATISTICS 
       
       ## Ridge regression 
       
       
       # Step 1: One-hot encoding the categorical variables on training dataset 
       
       # Here we reference the correct columns, ensuring 'Value' is the third column
       X <- model.matrix(Value ~ age_group + contract_left + International.Reputation + league + position_category + Potential + Wage + performance, 
                         data = training_set)[,-1]  # Remove intercept column
       
       # Step 2: Extract the response variable (Value), which is in the third column
       y <- training_set[, 3]  # Value is in the third column of training_set
       
       # Check the structure of X to ensure proper encoding
       head(X)
       
       # Step 3: Fit the Ridge Regression model
       ridge_model <- glmnet(X, y, alpha = 0)  # alpha = 0 indicates Ridge regression
       
       # Step 4: Perform cross-validation to find the optimal lambda
       cv_ridge <- cv.glmnet(X, y, alpha = 0)
       
       # View the optimal lambda chosen through cross-validation
       cv_ridge$lambda.min
       
       # Step 5: Make predictions using the optimal lambda
       predictions <- predict(ridge_model, s = cv_ridge$lambda.min, newx = X)
       
       # Step 6: View coefficients of the model (optional)
       coef(ridge_model)
       
       predictions_original_scale <- exp(predictions)
       y_original_scale <- exp(y)
       
       
       
       # Calculate MAE for the training set
       absolute_errors_train <- abs(predictions_original_scale - y_original_scale)
       mae_train <- mean(absolute_errors_train)
       print(paste("MAE on training set (original scale):", mae_train/1000000))
       
       #Calculate RMSE for training set
       rmse_test<- sqrt(mean((predictions_original_scale - y_original_scale)^2))
       print(paste("RMSE on test set:", rmse_test/1000000))
       
       
       #Calculate R squared for training set
       ss_total_train <- sum((y_original_scale - mean(y_original_scale))^2)
       ss_residual_train <- sum((y_original_scale - predictions_original_scale)^2)
       r_squared_train <- 1 - (ss_residual_train / ss_total_train)
       print(paste("R-squared on training set:", r_squared_train))
       
       
      #Calculate correlation for the training set
      correlation <- cor(predictions_original_scale, y_original_scale)
      print(paste("Correlation between actual and predicted values on training set (original scale):", correlation))
       
       
       
       
       
       ###  VALIDATION 
      
       # One-hot encoding for the catecorical varibales  on  test set using the same model matrix
       X_test <- model.matrix(Value ~ age_group + contract_left + International.Reputation + league + position_category + Potential + Wage + performance,  
                              data = test_set)[, -1]
       
       # Step 2: Extract the response variable (Value), which is in the third column
       y_test <- test_set[, 3]  # Value is in the third column of test_set
       
       # Check the structure of X_test to ensure proper encoding
       head(X_test)
       
       # Step 3: Fit the Ridge Regression model
       ridge_model <- glmnet(X_test, y_test, alpha = 0)  # alpha = 0 indicates Ridge regression
       
       # Step 4: Perform cross-validation to find the optimal lambda
       cv_ridge_test <- cv.glmnet(X_test, y_test, alpha = 0)
       
       # View the optimal lambda chosen through cross-validation
       cv_ridge_test$lambda.min
       
       # Make predictions on the test set
       predictions_test <- predict(ridge_model, s = cv_ridge_test$lambda.min, newx = X_test)
       
       # Exponentiate the predictions to return to the original scale
       test_predictions_original_scale <- exp(predictions_test)
       
       # Extract actual values from the test set (assuming 'Value' is the third column in test_set)
       actual_test_log <- log(test_set[, 3]) 
       y_test_original_scale <- exp(test_set[, 3])
       
       # Calculate MAE for the test set
       absolute_errors_test <- abs(test_predictions_original_scale -  y_test_original_scale)
       mae_test <- mean(absolute_errors_test)
       print(paste("MAE on test set (original scale):", mae_test/1000000))
       
       #Calculate RMSE for test set
       rmse_test<- sqrt(mean((test_predictions_original_scale - y_test_original_scale)^2))
       print(paste("RMSE on test set:", rmse_test/1000000))
       
       #Calculate R squared for test set
       ss_total_test <- sum((y_test_original_scale - mean(y_test_original_scale))^2)
       ss_residual_test <- sum((y_test_original_scale- test_predictions_original_scale)^2)
       r_squared_test <- 1 - (ss_residual_test / ss_total_test)
       print(paste("R-squared on test set:", r_squared_test))
       
       
       #Calculate correlation for the test set
       correlation_test <- cor(test_predictions_original_scale, y_test_original_scale)
       print(paste("Correlation between actual and predicted values on test set (original scale):", correlation_test))
       
       
    
       #  Plot Actual vs Predicted for test set
       plot(y_test_original_scale/1000000, test_predictions_original_scale/1000000, 
            xlab = "Actual Values", 
            ylab = "Predicted Values", 
            main = paste("Actual vs Predicted Values (Test Set) \nCorrelation:", round(correlation_test, 2)),
            pch = 16, col = "blue", cex = 0.7)
       abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2) 
       
       # Calculate the absolute percentage errors
       percentage_errors_test <- (abs(test_predictions_original_scale - y_test_original_scale) / abs(y_test_original_scale)) * 100
       
       # Calculate the Mean Absolute Percentage Error (MAPE)
       mape_test <- mean(percentage_errors_test)
       
       # Print the MAPE
       print(paste("MAPE on test set (original scale):", mape_test, "%"))
       
    
       
       
       ## RIDGE REGRESSION FOR EACH POSITION ( TO SEE WHETHER OUR OFFSET WILL IMPROVE) 
       
       # Initialize an empty list to store results
       mae_results <- list()
       
       # Step 1: One-hot encode the categorical variables for the whole training_set
       X_train <- model.matrix(Value ~ age_group + contract_left + International.Reputation + league + position_category + Potential + Wage + performance, 
                               data = training_set)[, -1]
       
       # Step 2: Extract the response variable (Value) from the training_set
       y_train <- training_set[, 3]  # Assuming Value is in the third column of training_set
       
       # Step 3: Fit the Ridge Regression model using the training_set
       ridge_model <- glmnet(X_train, y_train, alpha = 0)  # alpha = 0 indicates Ridge regression
       
       # Step 4: Perform cross-validation to find the optimal lambda for training_set
       cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0)
       
       # Check the best lambda from cross-validation
       best_lambda_ridge <- cv_ridge$lambda.min
       print(paste("Best lambda for Ridge Regression:", best_lambda_ridge))
       
       # Lasso Regression (alpha = 1) - Same cross-validation as Ridge
       cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1)  # Lasso regression (alpha = 1)
       best_lambda_lasso <- cv_lasso$lambda.min
       print(paste("Best lambda for Lasso Regression:", best_lambda_lasso))
       
       # Compare the models' performance
       cat("Best Lambda for Ridge:", best_lambda_ridge, "\n")
       cat("Best Lambda for Lasso:", best_lambda_lasso, "\n")
       
       # Use the best lambda from cross-validation for the final model fit (Ridge regression)
       final_model_ridge <- glmnet(X_train, y_train, alpha = 1, lambda = best_lambda_ridge)
       
       
       # Optimal lambda from cross-validation
       lambda_min <- cv_ridge$lambda.min
       
       # Step 5: Get unique positions from the training_set's position_category
       positions <- unique(training_set$position_category)
       
       # Loop through each position in the positions variable
       for (pos in positions) {
         # Step 6: Subset the test_set data based on the current position
         position_subset_test <- subset(test_set, position_category == pos)
         
         # Step 7: One-hot encode the categorical variables for the subset of the test_set data
         X_test <- model.matrix(Value ~ age_group + contract_left + International.Reputation + league + position_category + Potential + Wage + performance, 
                                data = position_subset_test)[, -1]
         
         # Step 8: Extract the response variable (Value) from the test_set
         y_test <- position_subset_test[, 3]  # Assuming Value is in the third column of test_set
         # Step 9: Make predictions using the previously trained Ridge model with the optimal lambda
         predictions_test <- predict(ridge_model, s = lambda_min, newx = X_test)
         
         # Step 10: Convert predictions and actual values to original scale (exp transformation)
         predictions_test_original_scale <- exp(predictions_test)
         y_test_original_scale <- exp(y_test)
         
         # Step 11: Calculate MAE for the current position subset in the test_set
         absolute_errors_test <- abs(predictions_test_original_scale - y_test_original_scale)
         mae_test <- mean(absolute_errors_test)
         
         # Store the result in the list for each position
         mae_results[[pos]] <- mae_test
       }
       # Step 12: Print MAE for each position in the test_set
       for (pos in positions) {
         print(paste("MAE for", pos, "in test set:", mae_results[[pos]]))
       }
       
       
       
       
       
       
       






