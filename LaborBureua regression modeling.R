#load packages
library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(tidyverse)
library(stargazer)

#load data
mydata <- read.csv("Data/EMPandRural.csv")
print(data)


#regression modeling
model <- lm( FLFPR_20to64_UNDER6 ~
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
            data = mydata)

summary(model)

urfemodel <- feols( FLFPR_20to64_UNDER6 ~
                         MFI + #median family income
                         H_UNDER6_SINGLEM + #number of households under 6 with single mothers
                         MCINFANT +
                         MCTODDLER +
                         MCPRESCHOOL +
                         MFCCINFANT +
                         MFCCTODDLER +
                         FEMP_M +
                         FEMP_SERVICE +
                         FEMP_SALES +
                         FEMP_N +
                         FEMP_P +
                         MFCCPRESCHOOL 
                         | COUNTY_NAME + STUDYYEAR,
                       data = filter(mydata, Urban == 0))

summary(urfemodel)

rurfemodel <- feols( FLFPR_20to64_UNDER6 ~
                      MFI + #median family income
                      H_UNDER6_SINGLEM + #number of households under 6 with single mothers
                      MCINFANT +
                      MCTODDLER +
                      MCPRESCHOOL +
                      MFCCINFANT +
                      MFCCTODDLER +
                      MFCCPRESCHOOL 
                    | COUNTY_NAME + STUDYYEAR,
                    data = filter(mydata, Urban == 1))

summary(rurfemodel)

library(car)
vif(lm(your_outcome ~ your_predictors, data = your_data))


ggplot(mydata, aes(x = MCINFANT, y = FLFPR_20to64_UNDER6)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ Urban)


library(plm)
#between effects model
pdata <- pdata.frame(mydata, index = c("COUNTY_NAME", "STUDYYEAR"))

be_model <- plm( FLFPR_20to64_UNDER6 ~
                     MFI + #median family income
                     H_UNDER6_SINGLEM + #number of households under 6 with single mothers
                     MCINFANT +
                     MCTODDLER +
                     MCPRESCHOOL +
                     MFCCINFANT +
                     MFCCTODDLER +
                     MFCCPRESCHOOL +
                   Urban, 
                 data = pdata,
                 model = "between")
summary(be_model)


#random effects
ra_model <- plm( FLFPR_20to64_UNDER6 ~
                   MFI + #median family income
                   H_UNDER6_SINGLEM + #number of households under 6 with single mothers
                   MCINFANT +
                   MCTODDLER +
                   MCPRESCHOOL +
                   MFCCINFANT +
                   MFCCTODDLER +
                   MFCCPRESCHOOL +
                   Urban, 
                 data = pdata,
                 model = "random")
summary(ra_model)

plot_data <- mydata %>%
  select(STUDYYEAR, FLFPR_20to64_UNDER6, MCINFANT, MCTODDLER, MCPRESCHOOL,
         MFCCINFANT, MFCCTODDLER, MFCCPRESCHOOL, FUNR_16) %>%
  group_by(STUDYYEAR) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(
    cols = -STUDYYEAR,
    names_to = "Variable",
    values_to = "Value"
  )
ggplot(plot_data, aes(x = STUDYYEAR, y = Value, color = Variable)) +
  geom_line(size = 1.2) +
  labs(
    title = "Female Labor Force Participation and Childcare Costs Over Time",
    x = "Year",
    y = "Mean Value",
    color = "Variable"
  ) +
  theme_minimal(base_size = 14)

flfpr_urban_rural <- mydata %>%
  filter(!is.na(Urban)) %>%
  group_by(STUDYYEAR, Urban) %>%
  summarise(mean_FLFPR = mean(FLFPR_20to64_UNDER6, na.rm = TRUE))

ggplot(flfpr_urban_rural, aes(x = STUDYYEAR, y = mean_FLFPR, color = factor(Urban))) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("0" = "#1D3557", "1" = "#A8DADC"), labels = c("Rural", "Urban")) +
  labs(
    title = "Female Labor Force Participation by Urban/Rural Status",
    x = "Year",
    y = "Mean Participation Rate (%)",
    color = "Area Type"
  ) +
  theme_minimal(base_size = 14)

emp_long <- mydata %>%
  select(STUDYYEAR, FEMP_M, FEMP_SERVICE, FEMP_SALES, FEMP_N, FEMP_P) %>%
  pivot_longer(
    cols = starts_with("FEMP_"),
    names_to = "Sector",
    values_to = "Employment"
  )

ggplot(emp_long, aes(x = STUDYYEAR, y = Employment, color = Sector)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  labs(
    title = "Female Employment by Industry Over Time",
    x = "Year",
    y = "Mean Female Employment",
    color = "Industry Sector"
  ) +
  theme_minimal(base_size = 14)

#Modeling different careers------------
m_femodel <- feols( FEMP_M ~
                       MFI + #median family income
                       H_UNDER6_SINGLEM + #number of households under 6 with single mothers
                       MCINFANT +
                       MCTODDLER +
                       MCPRESCHOOL +
                       MFCCINFANT +
                       MFCCTODDLER +
                       MFCCPRESCHOOL 
                     | COUNTY_NAME,
                     data = mydata)
summary(m_femodel)

SERVICE_femodel <- feols( FEMP_SERVICE~
                      MFI + #median family income
                      H_UNDER6_SINGLEM + #number of households under 6 with single mothers
                      MCINFANT +
                      MCTODDLER +
                      MCPRESCHOOL +
                      MFCCINFANT +
                      MFCCTODDLER +
                      MFCCPRESCHOOL 
                    | COUNTY_NAME ,
                    data = mydata)
summary(SERVICE_femodel)
 
 SALES_femodel <- feols( FEMP_SALES~
                              MFI + #median family income
                              H_UNDER6_SINGLEM + #number of households under 6 with single mothers
                              MCINFANT +
                              MCTODDLER +
                              MCPRESCHOOL +
                              MFCCINFANT +
                              MFCCTODDLER +
                              MFCCPRESCHOOL 
                            | COUNTY_NAME ,
                            data = mydata)
summary(SALES_femodel)

n_femodel <- feols( FEMP_N ~
                      MFI + #median family income
                      H_UNDER6_SINGLEM + #number of households under 6 with single mothers
                      MCINFANT +
                      MCTODDLER +
                      MCPRESCHOOL +
                      MFCCINFANT +
                      MFCCTODDLER +
                      MFCCPRESCHOOL 
                    | COUNTY_NAME,
                    data = mydata)
summary(n_femodel)

p_femodel <- feols( FEMP_P ~
                      MFI + #median family income
                      H_UNDER6_SINGLEM + #number of households under 6 with single mothers
                      MCINFANT +
                      MCTODDLER +
                      MCPRESCHOOL +
                      MFCCINFANT +
                      MFCCTODDLER +
                      MFCCPRESCHOOL 
                    | COUNTY_NAME,
                    data = mydata)
summary(p_femodel)

etable(
  m_femodel, SERVICE_femodel, SALES_femodel, n_femodel, p_femodel,
  headers = c("Management, Business, Sciences, and Arts", "Service", "Sales", "Natural Res.", "Production"),
  tex = FALSE
)

