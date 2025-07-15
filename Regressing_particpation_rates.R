#Packages-------
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(fixest)

VAdata <- read.csv("Data/VAdataparticipation.csv")
VAdata$Year <- as.integer(as.character(VAdata$Year))

VAdata22 <- VAdata %>%
  filter(Year == 2022)

VAdataTS <- VAdata %>%
  filter(Year >= 2014 & Year <= 2019)

#Modeling
model <- lm( Subsidy_Participation ~
               MFI + #median family income
               H_UNDER6_SINGLEM + #number of households under 6 with single mothers
               MCINFANT +
               MCTODDLER +
               MCPRESCHOOL +
               MFCCINFANT +
               MFCCTODDLER +
               MFCCPRESCHOOL +
               Urban + 
               ONERACE +
               ONERACE_W +
               ONERACE_B +
               ONERACE_I +
               ONERACE_A +
               ONERACE_H + 
               ONERACE_OTHER +
               TWORACES +
               HISPANIC,
             data = VAdata22)
summary(model)

#BASIC REGRESSION ON 2022
# Infant care price
model_infant <- lm(MCINFANT ~ Subsidy_Participation, data = VAdata22)
summary(model_infant)
# Toddler care price
model_toddler <- lm(MCTODDLER ~ Subsidy_Participation, data = VAdata22)
summary(model_toddler)
# Preschool price
model_preschool <- lm(MCPRESCHOOL ~ Subsidy_Participation, data = VAdata22)
summary(model_preschool)


long_data <- VAdata22 %>%
  pivot_longer(cols = c(MCINFANT, MCTODDLER, MCPRESCHOOL),
               names_to = "age_group",
               values_to = "price")
ggplot(long_data, aes(x = Subsidy_Participation, y = price, color = age_group)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Childcare Prices vs Subsidy Participation by Age Group",
       x = "Subsidy Participation Rate",
       y = "Price",
       color = "Age Group") +
  theme_minimal()



#FIXED REGRESSION ON TIME SERIES DATA
FEmodel_infant <- feols(MCINFANT ~ Subsidy_Participation | County + Year, data = VAdataTS)
summary(FEmodel_infant)

# Toddler care price
FEmodel_toddler <- feols(MCTODDLER ~ Subsidy_Participation| County + Year, data = VAdataTS)
summary(FEmodel_toddler)

# Preschool price
FEmodel_preschool <- feols(MCPRESCHOOL ~ Subsidy_Participation| County + Year, data = VAdataTS)
summary(FEmodel_preschool)


#LOG MODELS 22
model_infantlog <- lm(log(MCINFANT) ~ log(Subsidy_Participation), data = VAdata22)
summary(model_infantlog)

model_todlog <- lm(log(MCTODDLER) ~ log(Subsidy_Participation), data = VAdata22)
summary(model_todlog)

model_prelog <- lm(log(MCPRESCHOOL) ~ log(Subsidy_Participation), data = VAdata22)
summary(model_prelog)

#LOG FE Models FULL DATA
logFEmodel_preschool <- feols(log(MCPRESCHOOL) ~ log(Subsidy_Participation)| County + Year, data = VAdata)

summary(logFEmodel_preschool)

logFEmodel_infant <- feols(log(MCINFANT) ~ log(Subsidy_Participation)| County + Year, data = VAdata)
summary(logFEmodel_infant)

#Partcipation modeling
ogfemodel <- feols(log(Subsidy_Participation)~
                      MFI + #median family income
                      H_UNDER6_SINGLEM + #number of households under 6 with single mothers
                      log(MCINFANT) +
                      log(MCTODDLER) +
                      log(MCPRESCHOOL) +
                      log(MFCCINFANT) +
                      log(MFCCTODDLER) +
                      log(MFCCPRESCHOOL) +
                      ONERACE +
                      ONERACE_W +
                      ONERACE_B +
                      ONERACE_I +
                      ONERACE_A +
                      ONERACE_H + 
                      ONERACE_OTHER +
                      HISPANIC
                      | County + Year, data = VAdataTS)
summary(logfemodel)
