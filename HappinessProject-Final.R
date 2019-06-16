##### Happiness Project #####

  # Goal of this project is to build a machine learning model that will predict Happiness Score of a 
  # country based on different features like Economy (GDP per Capita), Family, Health (Life Expectancy), Freedom, Generosity, Government
  # corruption (or Trust on Government) and Dystopia
  
  ##### Initial Setup #####
  
      # LIBRARIES #
      
          # Following libraries will be used in this project
              library(corrplot)
              library(dslabs)
              library(dplyr)
              library(caret)
              library(ggplot2)
              library(randomForest)
              library(gam)
              library(ggplot2)

      # Load CSV File #

          # Load csv file into dataframe using "read.csv". "2017.csv" file is kept in "Documents" folder present in root directory
          
              happiness_17_df <- read.csv('~/Documents/2017.csv')
      
      # We will rename column names
      
          happiness_17_df <- happiness_17_df %>% rename('HappinessRank' = 'Happiness.Rank','HappinessScore'='Happiness.Score', 
                                                    'WhiskerHigh' = 'Whisker.high','WhiskerLow' = 'Whisker.low' ,
                                                    'Economy' = 'Economy..GDP.per.Capita.', 'Health' = 'Health..Life.Expectancy.',
                                                    'GovCorruption' = 'Trust..Government.Corruption.', 'Dystopia' = 'Dystopia.Residual')
      
      # Following are the new column names of happiness_17_df
      
      names(happiness_17_df) # Columns Names
      
      
      # We will create a new column Region in happiness_17_df dataframe and it can have 
      # following values: Asia, Europe, North America, South America, Australia, Middle East and Africa
      
          happiness_17_df <- happiness_17_df %>% mutate(Region = NA)
          
          happiness_17_df$Region[which(happiness_17_df$Country %in% c('Norway','Finland', 'Sweden','Austria','Belgium','Russia','Algeria','Romania','North Cyprus','Cyprus',
                                                                      'Turkey','Serbia','Croatia','Azerbaijan','Portugal','Bulgaria','Albania','Armenia','Georgia','Denmark',
                                                                      'Netherlands','Luxembourg','Spain','Poland','Latvia','Bolivia','Slovenia','Estonia','Kosovo','Bosnia and Herzegovina',
                                                                      'Iceland','Ireland','United Kingdom','Czech Republic','France','Belarus','Hungary','Montenegro','Greece','Ethiopia',
                                                                      'Switzerland','Germany','Slovakia','Italy','Lithuania','Moldova','Macedonia','Ukraine'))] <- 'Europe'
          
          happiness_17_df$Region[which(happiness_17_df$Country %in% c('Taiwan Province of China','Indonesia','Bhutan','Afghanistan','Singapore'
                                                                      ,'Malaysia','Vietnam','Bangladesh','Myanmar','India','Uzbekistan','Japan','South Korea','Turkmenistan','Hong Kong S.A.R., China'
                                                                      ,'China','Nepal','Mauritania','Thailand','Kazakhstan','Mauritius','Philippines','Pakistan','Tajikistan','Mongolia','Sri Lanka'))] <- 'Asia'
          
          happiness_17_df$Region[which(happiness_17_df$Country %in% c('United Arab Emirates','Saudi Arabia','Bahrain','Iraq','Jordan','Kyrgyzstan','Yemen','Israel','Qatar','Kuwait',
                                                                      'Palestinian Territories','Lebanon','Egypt','Iran','Syria'))] <- 'Middle East'
          
          happiness_17_df$Region[which(happiness_17_df$Country %in% c('Mexico','United States','Canada'))] <- 'North America'
          
          happiness_17_df$Region[which(happiness_17_df$Country %in% c('Guatemala','Haiti','Brazil','Panama','Belize'
                                                                      ,'Paraguay','Venezuela','Nicaragua','Peru','Honduras','Costa Rica','Chile','Argentina','Uruguay','Colombia','Ivory Coast'))] <- 'South America'
          
          happiness_17_df$Region[which(happiness_17_df$Country %in% c('Australia', 'New Zealand'))] <- 'Australia'
          
          happiness_17_df$Region[which(happiness_17_df$Country %in% c('El Salvador', 'Somalia','South Africa','Mozambique','Cambodia','Uganda','Chad','Guinea','Tanzania','Trinidad and Tobago',
                                                                      'Dominican Republic','Tunisia','Sierra Leone','Gabon','Congo (Kinshasa)','Sudan','Burkina Faso','Zimbabwe','Botswana','Togo',
                                                                      'Burundi','Malta','Nigeria','Cameroon','Namibia','Senegal','Mali','Ghana','Niger','Lesotho',
                                                                      'Benin','South Sudan','Rwanda','Central African Republic','Ecuador','Libya','Jamaica','Morocco','Kenya','Zambia',
                                                                      'Congo (Brazzaville)','Malawi','Angola','Madagascar','Liberia'))] <- 'Africa'
          
      # Change Region column to factor
      
          happiness_17_df$Region <- as.factor(happiness_17_df$Region)
      
      # Following code displays new structure of happiness_17_df
      
          str(happiness_17_df)


##### Data Exploration and Data Visualization #####

    # We can see that data is already clean and tidy:  
          head(happiness_17_df)
        
    # Following code provides details about happiness_17_df
    
        str(happiness_17_df) # Structure of happiness_17_df
        dim(happiness_17_df)   # 155 rows and 12 columns
    
    # Following are the top 10 countries based on Happiness Score:
    
        happiness_17_df[c(1:10),] %>% arrange(HappinessRank) %>% ggplot(aes(Country, HappinessScore)) + geom_point() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
        
        # Note that higher the Happiness Score of a country,  better the rank. e.g. Norway has the highest Happiness Score
        # and ranks 1st in the list
        # Happiness Score is the mean value of WhiskerHigh and WhiskerLow
        
        # 7 out of top 10 Happy countries in the world are from Europe
    
    # Following are the last 10 countries based on Happiness Score:
    
         happiness_17_df[c(145:155),] %>% arrange(HappinessRank) %>% ggplot(aes(Country, HappinessScore)) + geom_point() +
          theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
        # Note that many countries are from Africa region
    
    
    ##### Correlation between different attributes in happiness_17_df dataframe: #####
    
        # Following code will calculate correlation between different attributes
        # Note that we will consider only numeric features and we have excluded Happiness Rank as it will be inversely propotional to Happiness Score
         # and will have negative correlation
         
            # Happiness Rank is negatively correlation to Happiness Score
         
            cor(happiness_17_df[,c('HappinessScore',"HappinessRank")])
        
            happiness_correlation <- cor(happiness_17_df[,c('HappinessScore','Economy',
                                                            'Family','Health','Freedom','Generosity',
                                                            'GovCorruption','Dystopia')])
            
            # Round correlation values to nearest 2 decimal places and plot these values against each other
            
                corrplot(round(happiness_correlation,2), method = "number")
        
        # Findings from Correlation Plot
    
            # All values of features against Happiness Score are positive
            # We can see that Happiness Score is highly correlated to Economy, Family and Health.
            # Happiness Score is moderately correlated to Freedom, Trust (or government corruption) and Dystopla.
            # Happiness Score is loosely correlated to Generosity.
    
    
    ##### Following section shows scatter plots with regression line for different features vs Happiness Score : #####
    
        # These figures states that these features have linear relations with Happiness Score.  Additionally, higher the correction value of features, better is the linear relationship with Happiness Score
        
        # Following plot shows relationship between Happiness Score and Economy with regression line
        
            happiness_17_df %>% ggplot(aes(Economy,HappinessScore)) + geom_point() + geom_smooth()
        
            # Correlation value = 0.81 (High)
        
        # Following plot shows relationship between Happiness Score and Family with regression line
        
            happiness_17_df %>% ggplot(aes(Family,HappinessScore)) + geom_point() + geom_smooth()
        
            # Correlation value = 0.75 (High)
        
        # Following plot shows relationship between Happiness Score and Health with regression line
        
            happiness_17_df %>% ggplot(aes(Health,HappinessScore)) + geom_point() + geom_smooth()
        
            # Correlation value = 0.78 (High)
        
        # Following plot shows relationship between Happiness Score and Freedom with regression line
        
            happiness_17_df %>% ggplot(aes(Freedom,HappinessScore)) + geom_point() + geom_smooth()
        
            # Correlation value =  0.57 (Moderate)
        
        # Following plot shows relationship between Happiness Score and Generosity with regression line
        
            happiness_17_df %>% ggplot(aes(Generosity,HappinessScore)) + geom_point() + geom_smooth()
        
            # Correlation value = 0.16 (Low)
        
        # Following plot shows relationship between Happiness Score and GovCorruption with regression line
        
            happiness_17_df %>% ggplot(aes(GovCorruption,HappinessScore)) + geom_point() + geom_smooth()
        
            # Correlation value = 0.43 (Moderate)
        
        # Following plot shows relationship between Happiness Score and Dystopia with regression line
        
            happiness_17_df %>% ggplot(aes(Dystopia,HappinessScore)) %>% + geom_point() + geom_smooth()
        
            # Correlation value = 0.48 (Moderate)
        
        # We can see that these graphs show the same result: Happiness Score is highly correlated to Economy, Family and Health.  Moderately correlated to Freedom, Trust (or government corruption) and Dystopla.  Loosely correlated to Generosity.
        
    
    ##### Scatter Plot for different regions: #####
    
        # Following plot shows Happiness Score vs Economy for each region
        
            happiness_17_df %>% ggplot(aes(Economy,HappinessScore,color = Region)) + geom_point()
        
        # Following plot shows Happiness Score vs Family for each region
        
            happiness_17_df %>% ggplot(aes(Family,HappinessScore,color = Region)) + geom_point()
        
        # Following plot shows Happiness Score vs Health for each region
        
            happiness_17_df %>% ggplot(aes(Health,HappinessScore,color = Region)) + geom_point()
        
        # Following plot shows Happiness Score vs Freedom for each region  
        
            happiness_17_df %>% ggplot(aes(Freedom,HappinessScore,color = Region)) + geom_point()
        
        # Following plot shows Happiness Score vs Generosity for each region    
        
            happiness_17_df %>% ggplot(aes(Generosity,HappinessScore,color = Region)) + geom_point()
        
        # Following plot shows Happiness Score vs GovCorruption for each region  
        
            happiness_17_df %>% ggplot(aes(GovCorruption,HappinessScore,color = Region)) + geom_point()
        
        # Following plot shows Happiness Score vs Dystopia for each region
        
            happiness_17_df %>% ggplot(aes(Dystopia,HappinessScore,color = Region)) + geom_point()
            
    
    ##### Following graph displays Happiness Score in different regions: #####
    
        ggplot(happiness_17_df , aes(Region, HappinessScore)) + geom_boxplot(aes(fill=Region)) + theme_bw() +
          theme(axis.title = element_text(family = "Helvetica", size = (8)))
        
        # We can see that Australia region has highest mean Happiness Score as there are just 2 countries in that region. North America is the region with 2nd highest average Happiness Score as it has just 3 countries. South America comes at the 3rd place based on mean Happiness Score and Europe stands 4th in this list.
        # Following are the rankings of regions based on mean Happiness Score in the region
        # 1 Australia
        # 2 North America
        # 3 South America
        # 4 Europe
        # 5 and 6 Asia and Middle East are almost the same
        # 7 Africa
            
            
##### Training and Test Sets #####

    # We will create Train and Test set using happiness_17_df.  Note that we will train our models using 50% of the happiness_17_df data and then predict happiness score on the remaining 50% of the test data.
    
    # Following code will create Test and Train set
            # Note that we will only consider following features in train and test set: Economy,Family,Health,Freedom,Generosity,GovCorruption and Dystopia
    
        index <- createDataPartition(happiness_17_df$HappinessScore, times = 1, p = 0.5, list = FALSE)
        train_set <- happiness_17_df[-index,c("HappinessScore","Economy","Family","Health","Freedom","Generosity","GovCorruption", "Dystopia")]
        test_set <- happiness_17_df[-index,c("HappinessScore","Economy","Family","Health","Freedom","Generosity","GovCorruption", "Dystopia")]
    
    # Following code will display dimensions of train and test sets
    
        dim(train_set)
        dim(test_set)


##### RMSE FUNCTION #####

    # We are using mean squared error as the loss function to define the best approach to gauge model stability
    # Note that lower the RMSE,  better the prediction
    # Following RMSE (Root Mean Square Error) function will take 2 inputs, true and predicted 
    # ratings, and will return RMSE value.  This function will be used to calculate RMSE values 
    # for all the models
    
        RMSE <- function(true_ratings, predicted_ratings){
          sqrt(mean((true_ratings - predicted_ratings)^2))
        }

##### GLOBAL VARIABLES: RMSE result dataframe and Model Name #####

    # Following code will flush previous rmse_result, if this code is executed 2nd time
        if(exists('rmse_result') == TRUE)
        {
          if(length(rmse_result) > 0)
          {
              rm(rmse_result)  
          }
        }
        
    rmse_result <- data.frame(method = 'result datframe initialization' , RMSE = 0) # Initialization of rmse_result dataframe
    
    # Remove 1st entry of rmse_result, as its dummy
    
       rmse_result <- rmse_result[-1,0]
    
    # Model Name will be stored in following variable
    
        model_name <- ' '

        
##### RESULT FUNCTION #####
    # This function will add RMSE values into rmse_result
    # All models will call this function to report their RMSE values
    # Display rmse_result if display attribute is true
        
        add_RMSE_result <- function(model_name,RMSE_value,display){
          rmse_result <<- bind_rows(rmse_result , data.frame(method = model_name , RMSE = RMSE_value))
          if(display){
              rmse_result %>% knitr::kable()
          }
        }

##### Plot Smooth Density Graph Function #####
    # This function will take predicted ratings of a model and will make smooth denisity plot
    # Note that predicted ratings will be displayed against true ratings (i.e. test_set Happiness Score ratings)

    displaySmoothPlot <- function(pred){
      happiness_result <- data.frame(pred = pred, true = test_set$HappinessScore)
      happiness_result %>% ggplot(aes(true,pred)) + geom_point() + geom_smooth()
    }
        
        
##### Machine Learning Models #####

    ##### Model 1 - Regression #####
    model_name <- 'Model 1 : Regression'
    
        # In Data Exploration and Visualization section, we have seen that different features (Economy,Family,Health,Freedom,Generosity,GovCorruption,Dystopia) have linear relationship with Happiness Score
        # So we can use regression model to predict happiness score
        
        # Following code will provide us happiness_lm_hat using regression function "lm"
        
            happiness_lm_hat = lm(HappinessScore ~ . , train_set)
        
        # Following code will display summary of happiness_lm_hat
            
            summary(happiness_lm_hat)
        
        # Now we will use this model to predict Happiness Score for test set
        
            happiness_lm_pred <- predict(happiness_lm_hat, test_set)
        
        # Following is the mean squared error for liner regression model:
        
            MSE_lm <- RMSE(test_set$HappinessScore, happiness_lm_pred)
        
        # Add results into rmse_result
            
            add_RMSE_result(model_name,MSE_lm,TRUE)
            
        # Following code will generate Predicted vs True ratings graph with regression line
            
            displaySmoothPlot(happiness_lm_pred)
            
    ##### GLM Model #####
    model_name <- 'Model 2 : GLM'
    
        # Poisson glm regression is useful when we are predicting an outcome variable that represents counts from a set of continuous predictors
        
            # Following code will provide us happiness_lm_hat using train function of carets package
            
                happiness_glm_hat <- train(HappinessScore ~ . , data = train_set, method = 'glm')
            
            # Now we will use this model to predict Happiness Score for test set
                
                happiness_glm_pred <- predict(happiness_glm_hat, test_set)
            
            # Following is the mean squared error for liner regression model:
                
                RMSE_glm <- RMSE(test_set$HappinessScore,happiness_glm_pred)
            
        # Add results into rmse_result
              
            add_RMSE_result(model_name,RMSE_glm,TRUE)
            
        
        # Following code will generate Predicted vs True ratings graph with regression line
            
            displaySmoothPlot(happiness_glm_pred)
            

    ##### Loess Model #####
    model_name <- 'Model 3 : Loess'
    
        # Following code will tune span value that will provide lowest RMSE value
        # Note than loess function can accept maximum of 4 features. So, we have used the ones that are highly correlated to Happiness Score
    
            span <- c(seq(1:250))
    
            RMSE_span <- sapply(span, function(s){
              
              happiness_loess_hat <- loess(HappinessScore ~ Economy + Family + Health + Freedom, span = s, degree = 1 , data = train_set)
              
              happiness_loess_pred <- predict(happiness_loess_hat, test_set)
              
              MSE_loess <- RMSE(test_set$HappinessScore,happiness_loess_pred)
              
              return(MSE_loess)
            })
        
        # Following is the span that gives us the least RMSE value
            
            best_span <- span[which.min(RMSE_span)]
        
        # Following is the plot for RMSE vs span values
            
            plot(span,RMSE_span)
        
        # We will now use the best span value that we calculated above to find predicted and RMSE
            
            # Following code will provide us happiness_loess_hat using loess function
            
                happiness_loess_hat <- loess(HappinessScore ~ Economy + Family + Health + Freedom, data = train_set, span = best_span, degree = 1)
            
            # Now we will use this model to predict Happiness Score for test set
                
                happiness_loess_pred <- predict(happiness_loess_hat, test_set)
            
            # Following is the mean squared error:
                
                RMSE_loess <- RMSE(test_set$HappinessScore,happiness_loess_pred)
        
        # Add results into rmse_result
            
            add_RMSE_result(model_name,RMSE_loess,TRUE)
        
        # Following code will generate Predicted vs True ratings graph with regression line
            
            displaySmoothPlot(happiness_loess_pred)



    ##### KNN Model #####
    model_name <- 'Model 4 : knn'
    
       # Following code will provide us happiness_lm_hat using train function of carat package
        # We can tune value of k using tuneGrid attribute of train function
        # After multiple iterations, I found out the best tuneGrid that gives the least RMSE value.  I we changes tuneGrid, it increase RMSE
            
          happiness_knn_hat <- train(HappinessScore ~ . , data = train_set, method = 'knn' , tuneGrid = data.frame(k = seq(1,2,0.1)) )
      
      # Now we will use this model to predict Happiness Score for test set
          
          happiness_knn_pred <- predict(happiness_knn_hat, test_set)
          
      # Following is the mean squared error
      
          RMSE_knn <- RMSE(test_set$HappinessScore,happiness_knn_pred)
        
        # Add results into rmse_result
          
            add_RMSE_result(model_name,RMSE_knn,TRUE)
        
        # Following code will generate Predicted vs True ratings graph with regression line
            
            displaySmoothPlot(happiness_knn_pred)
    
            
    ##### Tree Model #####
    model_name <- 'Model 5 : Tree'	
    
        # Following code will tune cp value to give us the best estimate using train function of caret package
    
            happiness_rpart_hat <- train(HappinessScore ~ . , method = 'rpart' , 
                                         tuneGrid = data.frame(cp = seq(0,1,0.1)),
                                         data = train_set)
            
        # Following plot displays RMSE vs cp (Complexity Parameter)
            
            plot(happiness_rpart_hat)
        
            
        # Following is the Tree diagram based on the best model (with best cp value)
            
            plot(happiness_rpart_hat$finalModel, margin = 0.1)
            text(happiness_rpart_hat$finalModel, cex = 0.75)
        
        # Now we will use this model to predict Happiness Score for test set
            
            happiness_rpart_pred <- predict(happiness_rpart_hat, test_set)
        
        # Following is the mean squared error
            
            RMSE_tree <- RMSE(test_set$HappinessScore,happiness_rpart_pred)
        
        # Add results into rmse_result
            
            add_RMSE_result(model_name,RMSE_tree,TRUE)

        
        # Following code will generate Predicted vs True ratings graph with regression line
            
            displaySmoothPlot(happiness_rpart_pred)
        


    ##### Random Forest #####
    # Following are the 2 functions that implement Random Forest model
            
        ##### Random Forest : "RM" function #####
        model_name <- 'Model 6.1 : Random Forest - RF'
    
            # Following code will provide us happiness_randomforest_rm_hat using train function of caret package.  Note method name will be "rm" and we will tune mtry attribute of "rm" function
        
                happiness_randomforest_rf_hat <- train(HappinessScore ~ . , data = train_set, method = 'rf', tuneGrid = data.frame(mtry = seq(1,4,0.5)))
            
            # Following plot displays RMSE vs Randomly selected predictors
                
                plot(happiness_randomforest_rf_hat)
            
            # Now we will use this model to predict Happiness Score for test set
                
                happiness_randomforest_rf_pred <- predict(happiness_randomforest_rf_hat, test_set)
            
            # Following is the mean squared error
                
                RMSE_randomforest_rf <- RMSE(test_set$HappinessScore,happiness_randomforest_rf_pred)
            
            # Add results into rmse_result
                
                add_RMSE_result(model_name,RMSE_randomforest_rf,TRUE)
        
            
            # Following code will generate Predicted vs True ratings graph with regression line
                
                displaySmoothPlot(happiness_randomforest_rf_pred)


        ##### Random Forest : "RM" function #####
        model_name <- 'Model 6.2 : Random Forest - Rborist'
                
            # Following code will provide us happiness_randomforest_rborist_hat using train function of caret package.  Note method name will be "Rborist" and we will tune minNode attribute keeping predFixed constant of "Rborist" function
                
                happiness_randomforest_rborist_hat <- train(HappinessScore ~ . , data = train_set, method = 'Rborist', tuneGrid = data.frame(predFixed = 3,
                                                                                                                                             minNode = seq(1,5)))
                
            # Following plot displays RMSE vs Minimal Node Size
                
                plot(happiness_randomforest_rborist_hat)
                
            # Now we will use this model to predict Happiness Score for test set
                
                happiness_randomforest_rborist_pred <- predict(happiness_randomforest_rborist_hat, test_set)
                
            # Following is the mean squared error
                
                RMSE_randomforest_rborist <- RMSE(test_set$HappinessScore,happiness_randomforest_rborist_pred)
                
            # Add results into rmse_result
                
                add_RMSE_result(model_name,RMSE_randomforest_rborist,TRUE)
                
            # Following code will generate Predicted vs True ratings graph with regression line
                
                displaySmoothPlot(happiness_randomforest_rborist_pred)

            
    ##### GamLoess #####
    model_name <- 'Model 7 : GamLoess'
        
        # Following is the grid to tuen span and degree for gamloess function
            
            gamloess_grid <- expand.grid(span = seq(0,2,0.1), degree = 1)
        
        # Following code will provide us happiness_gamloess_hat by tuning our grid
            
            happiness_gamloess_hat <- train(HappinessScore ~ . , data = train_set,
                                        model = 'gamLoess',
                                        tuningGrid = gamloess_grid)
            
        #  Following plot displays RMSE vs Randomly Selected Predictors
            
            plot(happiness_gamloess_hat)
            
        # Now we will use this model to predict Happiness Score for test set
            
            happiness_gamloess_pred <- predict(happiness_gamloess_hat, test_set)
        
        # Following is the mean squared error
            
            RMSE_gamloess <- RMSE(test_set$HappinessScore,happiness_gamloess_pred)
        
        # Add results into rmse_result
            
            add_RMSE_result(model_name,RMSE_gamloess,TRUE)
        
        # Following code will generate Predicted vs True ratings graph with regression line
            
            displaySmoothPlot(happiness_gamloess_pred)


    ###### Multiple Models at a time #####
        
        # We can predict values for multiple models all at once.  Following is the code that will do this
        
        # Following is the list of models
            
            models <- c("svmLinear", 
                        "gamboost", "kknn", "gam",
                        "ranger", "avNNet", "mlp", "monmlp", "gbm",
                        "svmRadial", "svmRadialCost", "svmRadialSigma")
        
        # Following code will apply each model to get happiness_models_hat
        
            happiness_models_hat <- lapply(models, function(model){
              train(HappinessScore ~ . , data = train_set, method = model)
            })
        
            
        # Set column names of happiness_models_hat as model names
            
            names(happiness_models_hat) <- models
        
        # Now we will predict Happiness Score for test set
            
            happiness_models_pred <- sapply(happiness_models_hat, function(object) 
              predict(object, newdata = test_set))
            
        # Dimentions of happiness_models_pred. 76 rows and 17 columns(models)
            
            dim(happiness_models_pred)
        
        # Following code will calculate RMSE value of each model and will store it in RMSE_Models
            
            RMSE_Models <- sapply(models, function(model){
              print(model)
              return(RMSE(test_set$HappinessScore,happiness_models_pred[,model]))
            })
            
        # Add results into rmse_result
            
            for (i in 1:length(models)) {
              
              model_name <- paste('Model ',models[i])
              
              add_RMSE_result(model_name,RMSE_Models[models[i]],FALSE)
            }

        # rmse_result
        rmse_result %>% knitr::kable()
        
        
    ##### Ensemble #####
    model_name <- 'Model 8 : Ensemble'
        
        # Note that we are using predictions from "Multiple Models at a time" section.  Please execute that section 1st before executing following code.
        
        # Ensemble is used to combine 2 or more models to get better prediction
        # There are 3 main types of ensemble: Averaging, Majority Vote and Weighted Average
        
        # We will use Averaging to see if there are any improvements in our prediction
        
        # Following code will predict Happiness Rating using averaging for 'gbm' and 'svmRadial' models
            
            ensemble_models <- c('gbm','ranger')
          
            happiness_ensemble_pred <- rowMeans(happiness_models_pred[,c(ensemble_models)])
        
        # Following code displays individual results of 
            
            # We can see the RMSE reduced after applying Ensemble Averaging
            
                print(paste('gbm RMSE: ',RMSE(test_set$HappinessScore,happiness_models_pred[,'gbm'])))
                print(paste('ranger RMSE: ',RMSE(test_set$HappinessScore,happiness_models_pred[,'ranger'])))
                
                happiness_RMSE_ensemble <- RMSE(test_set$HappinessScore,happiness_ensemble_pred)
                print(paste('Ensemble RMSE: ',happiness_RMSE_ensemble))
            
        # Add results into rmse_result
            
            add_RMSE_result(model_name,happiness_RMSE_ensemble,TRUE)
            
        # Following code will generate Predicted vs True ratings graph with regression line
            
            displaySmoothPlot(happiness_ensemble_pred)
        
            
##### RESULTS #####
    # Following code will display RMSE values of all the models in ascending order of their RMSE
            
        rmse_result %>% arrange(RMSE)

##### CONCLUSION #####

