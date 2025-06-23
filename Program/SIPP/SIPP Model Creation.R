cat("\014")
rm(list = ls())

setwd("C:/Users/joshu/Desktop/DSPG/Code/SIPP")



library("data.table")
library("bit64")
library("dplyr")
library("stargazer")

data <- read.csv("cleaneddata.csv")

#Notes
#Use EPAYHELP Because more real responses

data <- data %>% 
  filter(kid1 == 1)

#Making Factors
factor_cols <- c("ESEX", "TEHC_METRO", "EWHOPAID1", "EducationLevel", "SingleHouse",
                 "age_group1", "age_group2", "age_group3", "age_group4", "age_group5",
                 "age_group6")
data[factor_cols] <- lapply(data[factor_cols], factor)
data <- data %>% 
  mutate(FemaleSingleParent = if_else(ESEX == "Female" & SingleHouse == "Single Parent",1,0))

summary(data)
#Creating the Model

#First model thought of, looking at interaction between age of kids
TimeLostModel <- lm(TimeLost~TAGE+TEHC_METRO+EWHOPAID1+ESEX+SingleHouse+
                      kid2+kid3+
                      age_group1 + kid2:age_group2 + kid3:age_group3, 
                    data)
summary(TimeLostModel)
stargazer(TimeLostModel)
summary(data)

#Second Model, looking at interaction between rural and kids
TimeLostModel2 <- lm(TimeLost~TAGE+TEHC_METRO+EWHOPAID1+ESEX+SingleHouse+
                        kid2*TEHC_METRO + kid3*TEHC_METRO,data)
summary(TimeLostModel2)

#3rd Model, No Interactions
TimeLostModel3 <- lm(TimeLost~TAGE+TEHC_METRO+EWHOPAID1+ESEX+SingleHouse+
                       kid2+kid3+kid4+kid5,data)
summary(TimeLostModel3)
stargazer(TimeLostModel3)

# 4th Model, Interact for female and solo household (not working, seems like perfect collinearity)
TimeLostModel4 <- lm(TimeLost~TAGE+TEHC_METRO+EWHOPAID1+ESEX+SingleHouse+
                       +FemaleSingleParent+
                       kid2+kid3+kid4+kid5,data)
summary(TimeLostModel4)

summary(data$ESEX[data$SingleHouse == "Single Parent"])
data <- droplevels(data)
table(data$ESEX, data$SingleHouse, useNA = "ifany")

#Hours Worked Models
HoursWorkedModel <- lm(TMWKHRS~TAGE+TEHC_METRO+EWHOPAID1+ESEX+SingleFamily+
                         kid1+kid2+kid3+kid4+kid5+kid6+
                         kid1:age_group1 + kid2:age_group2 + kid3:age_group3 + 
                         kid4:age_group4 + kid5:age_group5 + kid6:age_group6, 
                       data)
summary(HoursWorkedModel)




#Creating Model with only Working age individuals
data2 <- data %>% 
  filter(TAGE<= 45)


TimeLostModel2 <- lm(TimeLost~TAGE+TEHC_METRO+EWHOPAID1+ESEX+
                       kid1+kid2+kid3+kid4+kid5+kid6+
                       kid1:age_group1 + kid2:age_group2 + kid3:age_group3 + 
                       kid4:age_group4 + kid5:age_group5 + kid6:age_group6, 
                     data2)
summary(TimeLostModel2)





#### Hurdle 2 Part Model for Time Lost
data3 <- data %>% 
  mutate(TimeLostFactor = if_else(TimeLost > 0,100,0))
data3$ESEX <- factor(data3$ESEX, levels = c("Female", "Male"))
data3$ESEX <- relevel(data3$ESEX, ref = "Male")
data3$TEHC_METRO   <- factor(data3$TEHC_METRO)


Hurdle1 <- lm(TimeLostFactor~TAGE+TEHC_METRO+EPAYHELP+ESEX+SingleFamily+
                       kid1+kid2+kid3+kid4+kid5+kid6,data3)
summary(Hurdle1)

summary(data3$TEHC_METRO)

#Part 2
data4 <- data3 %>% 
  filter(TimeLostFactor == 100) %>% 
  select(TimeLost,TAGE,TEHC_METRO,EPAYHELP,ESEX,SingleFamily,
           kid1,kid2,kid3,kid4,kid5)
data4$TEHC_METRO   <- factor(data4$TEHC_METRO)
data4$EPAYHELP    <- factor(data4$EPAYHELP)


Hurdle2 <- lm(TimeLost~TAGE+TEHC_METRO+EPAYHELP+ESEX+SingleFamily+
                         kid1+kid2+kid3+kid4+kid5,data4)
summary(Hurdle2)


stargazer(Hurdle1, Hurdle2)
mean(data4$TimeLost[data4$TEHC_METRO == "Metro"])
mean(data4$TimeLost[data4$TEHC_METRO != "Metro"])









