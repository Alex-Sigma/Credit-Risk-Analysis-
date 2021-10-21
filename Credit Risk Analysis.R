
## Example: Identifying Risky Bank Loans ----
## Step 2: Exploring and preparing the data ----
# This code and date set was taken reproduced and adjusted from the book 
# was provided by from the "Machine leanrning with R Third addition"

# Downloading the Libraries
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)
library(h2o)
library(glue)
library(cowplot)
library(recipes)


source <- "00_Data/credit.csv"
credit <- read.csv("00_Data/credit.csv")


credit %>% str()
credit %>%  head() %>% view()
# look at two characteristics of the applicant
table(credit$checking_balance)
table(credit$savings_balance)

# look at two characteristics of the loan
summary(credit$months_loan_duration)
summary(credit$amount)

# look at the class variable
table(credit$default)

# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
set.seed(123)
train_sample <- sample(1000, 900)

str(train_sample)

# split the data frames
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

# check the proportion of class variable
prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## Step 3: Training a model on the data ----
# build the simplest decision tree
credit
library(C50)
credit_model <- C5.0(credit_train[-17], credit_train$default %>% as.factor())

# display simple facts about the tree
credit_model

# display detailed information about the tree
summary(credit_model)

## Step 4: Evaluating model performance ----
# create a factor vector of predictions on test data
credit_pred <- predict(credit_model, credit_test)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## Step 5: Improving model performance ----

## Boosting the accuracy of decision trees
# boosted decision tree with 10 trials
credit_boost10 <- C5.0(credit_train[-17], credit_train$default %>% as.factor(),
                       trials = 10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## Making some mistakes more costly than others

# create dimensions for a cost matrix
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

# build the matrix
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2, dimnames = matrix_dimensions)
error_cost

# apply the cost matrix to the tree
credit_cost <- C5.0(credit_train[-17], credit_train$default %>% as.factor(),
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# Data preprocessing -----

# Data Reprocessing the recepies----
#Plan
#1. Zero Vaiance 
# 2. Transformation
# 3. Discretize
# 4. Dummy Variables
# 5. Interaction variables/ Engineered Features
# 6. Normalization (Zeit, Seiten)
# 7. Multivariate Transformation (Positionen, Seiten )

recipe_obj<- recipe(default~., data=credit_train) %>%
    step_zv(all_predictors()) %>%
    step_center(all_numeric()) %>%
    step_scale(all_numeric()) %>%
    step_dummy(all_nominal()) %>%
    prep()



# Final recipe----

recipe_obj

train_tbl<- bake(recipe_obj, new_data = credit_train)

train_tbl %>% glimpse()

test_tbl<- bake(recipe_obj, new_data = credit_train)

test_tbl %>%
    glimpse()

data         <- train_tbl
feature_expr <- quo(default)

get_cor <- function(data, target, use = "pairwise.complete.obs",
                    fct_reorder = FALSE, fct_rev = FALSE) {
    
    feature_expr <- enquo(target)
    feature_name <- quo_name(feature_expr)
    
    data_cor <- data %>%
        mutate_if(is.character, as.factor) %>%
        mutate_if(is.factor, as.numeric) %>%
        cor(use = use) %>%
        as_tibble() %>%
        mutate(feature = names(.)) %>%
        select(feature, !! feature_expr) %>%
        filter(!(feature == feature_name)) %>%
        mutate_if(is.character, as_factor)
    
    if (fct_reorder) {
        data_cor <- data_cor %>%
            mutate(feature = fct_reorder(feature, !! feature_expr)) %>%
            arrange(feature)
    }
    
    if (fct_rev) {
        data_cor <- data_cor %>%
            mutate(feature = fct_rev(feature)) %>%
            arrange(feature)
    }
    
    return(data_cor)
    
}

train_tbl %>%
    get_cor(default_yes, fct_reorder = T, fct_rev = T)

plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE,
                     include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
                     size = 2, line_size = 1, vert_size = 1,
                     color_pos = palette_light()[[1]],
                     color_neg = palette_light()[[2]]) {
    
    feature_expr <- enquo(target)
    feature_name <- quo_name(feature_expr)
    
    data_cor <- data %>%
        get_cor(!! feature_expr, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
        mutate(feature_name_text = round(!! feature_expr, lbl_precision)) %>%
        mutate(Correlation = case_when(
            (!! feature_expr) >= 0 ~ "Positive",
            TRUE                   ~ "Negative") %>% as.factor())
    
    g <- data_cor %>%
        ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) +
        geom_point(aes(color = Correlation), size = size) +
        geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
        geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
        expand_limits(x = c(-1, 1)) +
        theme_tq() +
        scale_color_manual(values = c(color_neg, color_pos))
    
    if (include_lbl) g <- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
    
    return(g)
    
}


train_tbl %>%
    #select(default_yes, contains("JobRole")) %>%
    plot_cor(target = default_yes, fct_reorder = T, fct_rev = F)

plot_cor(train_tbl, target = Attrition_Yes, fct_reorder = T)

# H2O MODELLING ----
# HR 201: PREDICTING EMPLOYEE ATTRITION WITH H2O AND LIME ----
# CHAPTER 4: H2O MODELING ----


# create a random sample for training and test data
# use set.seed to use the same random number sequence as the tutorial
set.seed(123)
train_sample <- sample(1000, 900)

str(train_sample)

# split the data frames
credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]


# ML Preprocessing
recipe_obj<-recipe(default~., data=credit_train) %>%
    step_zv(all_predictors()) %>%
    prep()



train_tbl<- bake(recipe_obj,new_data = credit_train)
test_tbl<- bake(recipe_obj,new_data = credit_test)
glimpse(train_tbl)
glimpse(test_tbl)


# 2. Modelling ----

h2o.init()


split_h2o <- h2o.splitFrame(as.h2o(train_tbl), ratios = c(0.85), seed = 1234)
train_h2o <- split_h2o[[1]]
valid_h2o <- split_h2o[[2]]
test_h2o <- as.h2o(test_tbl)



y<-"default"
x <- setdiff(names(train_h2o),y)

automl_models_h2o <- h2o.automl(
    x=x,
    y=y,
    training_frame = train_h2o,
    validation_frame = valid_h2o,
    leaderboard_frame = test_h2o,
    max_runtime_secs = 30,
    nfolds = 5
)

typeof(automl_models_h2o)
slotNames(automl_models_h2o)
automl_models_h2o@leaderboard

automl_models_h2o@leader

h2o.getModel("GLM_1_AutoML_1_20211021_200313")



automl_models_h2o@leaderboard %>% 
    
    as.tibble() %>% 
    slice(1) %>% 
    pull(model_id) %>% 
    
    h2o.getModel()

extract_h2o_model_name_by_postion <- function(h2o_leaderboard, n=1, verbose=T){
   model_name <-  h2o_leaderboard %>% 
     as.tibble() %>% 
        slice(n) %>% 
        pull(model_id) %>% 
        
       if(verbose) message(model_name)
   return(model_name)
    
    
}

automl_models_h2o@leaderboard %>% 
    
    extract_h2o_model_name_by_postion(2) 
    
    
    h2o.getModel("XGBoost_2_AutoML_1_20211021_200313" ) %>% 
    h2o.saveModel("04_Modeling/")

    h2o.loadModel("04_Modeling/XGBoost_2_AutoML_1_20211021_200313")

    #Making predictions '    
    xgboost_h2o <- h2o.loadModel("04_Modeling/XGBoost_2_AutoML_1_20211021_200313")

    
    xgboost_h2o
    
    predictions <- h2o.predict(xgboost_h2o, newdata=test_h2o)
predictions_tbl <- predictions %>% 
    as.tibble()
predictions_tbl    


# 3. Visualizing The Leaderboard ----

data_transformed <- automl_models_h2o@leaderboard %>%
    as.tibble() %>%
    mutate(model_type = str_split(model_id, "_", simplify = T)[,1]) %>%
    slice(1:10) %>%
    rownames_to_column() %>%
    mutate(
        model_id   = as_factor(model_id) %>% reorder(auc),
        model_type = as.factor(model_type)
    ) %>%
    gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T) %>% 
    mutate(model_id=paste0(rowname,". ", model_id) %>% as_factor() %>% fct_rev)

data_transformed %>%
    ggplot(aes(value, model_id, color = model_type)) +
    geom_point(size = 3) +
    geom_label(aes(label = round(value, 2), hjust = "inward")) +
    facet_wrap(~ key, scales = "free_x") +
    theme_tq() +
    scale_color_tq() +
    labs(title = "H2O Leaderboard Metrics",
         subtitle = paste0("Ordered by: auc"),
         y = "Model Postion, Model ID", x = "")


h2o_leaderboard <- automl_models_h2o@leaderboard

plot_h2o_leaderboard <- function(h2o_leaderboard, order_by = c("auc", "logloss"),
                                 n_max = 20, size = 4, include_lbl = TRUE) {
    
    # Setup inputs
    order_by <- tolower(order_by[[1]])
    
    leaderboard_tbl <- h2o_leaderboard %>%
        as.tibble() %>%
        mutate(model_type = str_split(model_id, "_", simplify = T)[,1]) %>%
        rownames_to_column(var = "rowname") %>%
        mutate(model_id = paste0(rowname, ". ", as.character(model_id)) %>% as.factor())
    
    # Transformation
    if (order_by == "auc") {
        
        data_transformed_tbl <- leaderboard_tbl %>%
            slice(1:n_max) %>%
            mutate(
                model_id   = as_factor(model_id) %>% reorder(auc),
                model_type = as.factor(model_type)
            ) %>%
            gather(key = key, value = value,
                   -c(model_id, model_type, rowname), factor_key = T)
        
    } else if (order_by == "logloss") {
        
        data_transformed_tbl <- leaderboard_tbl %>%
            slice(1:n_max) %>%
            mutate(
                model_id   = as_factor(model_id) %>% reorder(logloss) %>% fct_rev(),
                model_type = as.factor(model_type)
            ) %>%
            gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T)
        
    } else {
        stop(paste0("order_by = '", order_by, "' is not a permitted option."))
    }
    
    # Visualization
    g <- data_transformed_tbl %>%
        ggplot(aes(value, model_id, color = model_type)) +
        geom_point(size = size) +
        facet_wrap(~ key, scales = "free_x") +
        theme_tq() +
        scale_color_tq() +
        labs(title = "Leaderboard Metrics",
             subtitle = paste0("Ordered by: ", toupper(order_by)),
             y = "Model Postion, Model ID", x = "")
    
    if (include_lbl) g <- g + geom_label(aes(label = round(value, 2), hjust = "inward"))
    
    return(g)
    
}

automl_models_h2o@leaderboard %>%
    plot_h2o_leaderboard(order_by = "logloss")    
    