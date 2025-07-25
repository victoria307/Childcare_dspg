## File Setup ----
# Cleaning Enviorment ----
rm(list=ls())
cat("\014")

# Getting Necessary Packages ----
library("data.table")
library("bit64")
library("dplyr")
library("tidyr")
library("forcats")
library(stringr)
library(httr)
library(R.utils)
library(fixest)
library(caret)
library(fastDummies)
library(viridis)

# Loading in data ----
#gunzip("Data/cps_00006.csv.gz")

data <- read.csv("Data/cps_00006.csv")

#Setting up data
data$CPSID <- as.character(data$CPSID)
data$CPSIDP <- as.character(data$CPSIDP)

#Removing Columns we will not use (NAs lead to the ML model breaking)
data <- data %>% 
  dplyr::select(-c(ASECFLAG, ASECWTH, HWTFINL, WTFINL, ASECWT, UHRSWORK1))

# Data Cleaning ----
data <- data %>%
  filter(NCHLT5 > 0) %>% 
  mutate(
    Education = case_when(
      EDUC <= 72 ~ "No HS",
      EDUC == 73 ~ "HS",
      EDUC > 73 & EDUC <= 110 ~ "Some College",
      EDUC == 111 ~ "Bachelors",
      EDUC > 111 ~ "Graduate/Professional Degree",
      TRUE ~ NA
    ),
    
    EMP = case_when(
      EMPSTAT == 10 | EMPSTAT == 12 ~ 1,
      TRUE ~ 0
    ),
    EMPSPOUSE = case_when(
      EMPSTAT_SP == 10 | EMPSTAT_SP == 12 ~ 1,
      TRUE ~ 0
    ),
    
    Married = if_else(MARST == 1,1,0),
    
    Female = if_else(SEX ==2,1,0),
    
    Post = if_else(YEAR > 2022 | (YEAR == 2022 & MONTH >= 7),1,0),
    
    Subsidy = case_when(
      FAMSIZE == 2 & FAMINC >= 730 & FAMINC <= 820 ~ "Newly_Eligible",
      FAMSIZE == 3 & FAMINC >= 740 & FAMINC <= 830 ~ "Newly_Eligible",
      FAMSIZE == 4 & FAMINC >= 820 & FAMINC <= 841 ~ "Newly_Eligible",
      FAMSIZE == 5 & FAMINC >= 830 & FAMINC <= 841 ~ "Newly_Eligible",
      
      FAMSIZE == 2 & FAMINC < 730 ~ "Always_Eligible",
      FAMSIZE == 3 & FAMINC < 740  ~"Always_Eligible",
      FAMSIZE == 4 & FAMINC < 820 ~ "Always_Eligible",
      FAMSIZE == 5 & FAMINC < 830 ~ "Always_Eligible",
      
      FAMSIZE == 2 & FAMINC > 820 ~ "Never_Eligible",
      FAMSIZE == 3 & FAMINC > 830 ~ "Never_Eligible",
      FAMSIZE == 4 & FAMINC > 841 ~ "Never_Eligible",
      FAMSIZE == 5 & FAMINC > 841 ~ "Never_Eligible",
      TRUE ~ NA
    ),
    
    Race = case_when(
      RACE == 100 ~ "White",
      RACE == 200 ~ "Black",
      RACE == 300 ~ "American Indian",
      RACE == 600 | RACE == 651  | RACE == 652 ~ "Asian or Pacific Islander",
      RACE == 999 ~ NA,
      RACE >= 800 ~ "2+ Race",
      TRUE ~ NA
    ),
    
    Rural = if_else(METRO == 3,1,0)
    
    
  )


data <- data %>%
  filter(if_all(everything(), ~ !is.na(.)),
         !is.na(RACE)) %>% 
  mutate(across(where(is.character), as.factor))



data2 <- data %>% 
  dplyr::select(-c(SERIAL, CPSID, STATEFIP,
                   METRO, PERNUM, CPSIDP, CPSIDV, SEX, RACE,
                   MARST, EMPSTAT, EDUC, FAMINC, EMPSTAT_SP))






#ML Model ----
set.seed("12345")
index <- createDataPartition(data2$Subsidy, p = 0.7, list = FALSE)
train_data <- data2[index,]
test_data <- data2[-index,]

#Creating Groups
x_raw <- train_data %>%
  dplyr::select(-Subsidy)
dummies <- dummyVars(" ~ .", data = x_raw, fullRank = TRUE)


x_train <- predict(dummies, newdata = x_raw) %>% as.data.frame()
x_test_raw <- test_data %>%
  dplyr::select(-Subsidy)
x_test <- predict(dummies, newdata = x_test_raw) %>% as.data.frame()

y_train <-  train_data %>% 
  dplyr::select(Subsidy) 

y_test <-  test_data %>% 
  dplyr::select(Subsidy)


# 3. Training the model(s) on the training data
metric <- "Accuracy"
ctrl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)
library(doParallel)
cl <- makeCluster(parallel::detectCores() - 1)
registerDoParallel(cl)
# linear algorithm 
fit.lda <- train(x_train, y_train$Subsidy, method = "lda", metric = metric, trControl = ctrl)
fit.cart <- train(x_train, y_train$Subsidy, method = "rpart", metric = metric, trControl = ctrl)
fit.knn <- train(x_train, y_train$Subsidy, method = "knn", metric = metric, trControl = ctrl)
fit.svm <- train(x_train, y_train$Subsidy, method = "svmRadial", metric = metric, trControl = ctrl)
fit.rf <- train(x_train, y_train$Subsidy, method = "rf", metric = metric, trControl = ctrl)



saveRDS(fit.rf, file = "Program/CPS Models/fit_rf_model.rds")

stopCluster(cl)
registerDoSEQ()


results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

#Looking at Out of Sample Accuracy

library(caret)

# Create a named list of models
models <- list(
  lda = fit.lda,
  cart = fit.cart,
  knn = fit.knn,
  svm = fit.svm,
  rf = fit.rf
)

# Initialize empty lists to store results
out_sample_results <- lapply(models, function(model) {
  pred <- predict(model, newdata = x_test)
  cm <- confusionMatrix(pred, y_test$Subsidy)
  data.frame(
    Accuracy = cm$overall["Accuracy"],
    Kappa = cm$overall["Kappa"]
  )
})

# Combine into a single data frame
out_sample_df <- do.call(rbind, out_sample_results)
out_sample_df <- round(out_sample_df, 3)
out_sample_df$Model <- rownames(out_sample_df)
rownames(out_sample_df) <- NULL

# Show results
out_sample_df


# Creating Visual for ML Model ----

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(extrafont)



model_long <- out_sample_df %>%
  pivot_longer(cols = c(Accuracy, Kappa), names_to = "Metric", values_to = "Value")

# Plot

ggplot(model_long, aes(x = reorder(Model, Value), y = Value, fill = Model)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(Value, 3)), vjust = -0.5, size = 4.5) +
  facet_wrap(~ Metric, scales = "free_y") +
  #Mako for Website (G) and rocket for Poster
  scale_fill_viridis_d(option = "rocket") +
  scale_y_continuous(limits = c(0, 1))+
  labs(title = "Model Performance Comparison Predicting Income",
       x = "Model",
       y = "Score") +
  theme_bw(base_size = 15, base_family = "Times New Roman") +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 16, family = "Times New Roman"),
    axis.text = element_text(family = "Times New Roman"),
    axis.title = element_text(family = "Times New Roman"),
    plot.title = element_text(family = "Times New Roman", hjust = 0.5))



out_sample_df