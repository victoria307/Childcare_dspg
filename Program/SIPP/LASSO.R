cat("\014")
rm(list = ls())

setwd("C:/Users/joshu/Desktop/DSPG/Code/SIPP")

#install.packages("glmnet")
library(glmnet)
library("data.table")
library("bit64")
library("dplyr")

data <- read.csv("cleaneddata.csv")
data <- data[complete.cases(data[, c("TAGE", "ESEX", "TEHC_METRO", "TEHC_REGION", 
                                     "EPAYHELP", "kid1", "kid2", "kid3",
                                     "kid4", "kid5", "kid6", "EducationLevel",
                                     "SingleHouse")]), ]

HoursLostAgeInteraction <- model.matrix( ~ TAGE + ESEX + TEHC_METRO + 
                                          EPAYHELP + kid1 + kid2 +
                                          kid3 + kid4 + kid5 + kid6 + EducationLevel +
                                          SingleHouse, data)[, -1]
TimeLost <- data$TimeLost

lasso_model1 <- cv.glmnet(HoursLostAgeInteraction,TimeLost , alpha = 1)
best_lambda1 <- lasso_model1$lambda.min
best_model1 <- glmnet(HoursLostAgeInteraction,TimeLost , alpha = best_lambda1)
coef(best_model1)