# Load Packages ---------------------------------------------------------
library(shiny)
library(tidyverse)
library(readxl)
library(tigris)
library(leaflet)
library(sf)
library(plotly)
library(shinydashboard)
library(echarts4r)
library(timevis)  # added for timeline
library(DT)
library(kableExtra)


options(tigris_use_cache = TRUE)

# Data --------------------------------------------------------------------
getwd()
# Import data
labor_data <- read_csv("Data/WB Output/VA_data.csv")
# Create VA data
VA_data <- labor_data %>%
  filter(STATE_NAME == "Virginia")

# Prepare VA counties spatial data
va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  mutate(FIPS = GEOID) %>%
  st_transform(crs = 4326)

# Format FIPS codes consistently
VA_data <- VA_data %>%
  mutate(COUNTY_FIPS_CODE = str_pad(as.character(COUNTY_FIPS_CODE), width = 5, pad = "0"))

# Join spatial and attribute data
map_data_all <- va_counties %>%
  left_join(VA_data, by = c("GEOID" = "COUNTY_FIPS_CODE"))

VDOE_data <- read_csv("Data/VDOE+WB+Rural.csv")
total_by_date <- VDOE_data %>%
  group_by(date) %>%
  summarise(Total_Count = sum(Total_Count, na.rm = TRUE)) %>%
  ungroup()

# Policy data moved outside UI -------------------------------------------------
policy_data <- data.frame(
  id = 1:13,
  content = c(
    "2018 Reimbursement Rate Increase", 
    "2020 CCDF Funding Allocation",
    "2020 CCDF Transition Plan",
    "2021 COVID-19 Relief",
    "2021 House Bill 2206",
    "2021 House Bill 1800",
    "2022 House Bill 30",
    "2022 Supplier Correspondence on Copays",
    "2022 Supplier Correspondence on Revisions to Provider Payment Rates",
    "2023 Correspondence on Update in Paid Childcare Absences",
    "2024 HB419",
    "2024 Provider Correspondence on Waitlists Being Reinstated",
    "2024 HB627"
  ),
  start = as.Date(c(
    "2018-06-01",
    "2020-10-01",
    "2020-11-24",
    "2021-02-22",
    "2021-01-28",
    "2021-01-01",
    "2022-01-01",
    "2022-12-06",
    "2022-09-29",
    "2023-09-20",
    "2024-02-07",
    "2024-06-04",
    "2024-01-10"
  )),
  end = as.Date(c(
    NA,  # ended policy
    NA,  # ongoing
    NA, # ended policy
    "2023-01-01",
    NA,
    NA,
    "2024-07-01",
    NA,
    NA,
    NA,
    NA,
    NA,
    NA
    )),
  
  summary = c(
    "In 2018, Virginia increased reimbursement rates for licensed providers to align more closely with the 70th percentile of the market rate survey.",
    "In 2020, a report was released explaining funding allocation within CCDF.",
    "The CCDF was transitioned from the Virginia Department of Social Services (VDSS) to the Virginia Department of Education (VDOE). The plan included a phased shift to bringing all aspects of subsidies to the VDOE. It also brought up the shift to Virginia Quality Inititiavies (VQI) and how it will be implemented in phases, along with impact of COVID-19 on childcare and updates on supporting CCDF through COVID-19.",
    "In 2021 the CCDF was given funds under the Coronavirus Response and Relief Supplemental appropriations Act (CRRSA). The goals to utilize these funds was to increase the supply of childcare, increase resources for providers and families, expand requirements for subsidies, and to increase access to childcare in underserved communities. Copayments were eliminated and other eligibility requirements would soon be changing as well. *The shaded region below represents the timeframe copayments to providers were eliminated.",
    "A bill temporarily expanding subsidy program to those who are actively looking for work and raising the qualifying threshhold to those who's income is below 85% of the state median income.",
    "Provided 9.6 Million Dollars to CCDF to expand the subsidy program.",
    "This bill maximized the use of federal CCDF funds to eliminate the waitlist for subsidies. It also continued the 85% of federal povery line eligibility requirement to the 2022-2024 biennium. *The shaded region below represents the timeframe waitlists for subsidies were eliminated.",
    "This correspondence informed subsidy providers that copays will be reinstated January 1 2023, and brought up that with it would be a new copayment scale.",
    "This correspondence informed subsidy providers that payment rates would be changing to fit a new cost estimation model, giving a majority of centers increased rates. It also mentioned new copayment scales, along with subsidy providers now being paid for 15 days of planned holidays.",
    "This correspondence effectively changed the number of allotted absence days still paid to the provider from 36 to 60 days a year.",
    "This bill amended the codes that handled funding for childcare, teducators indecisive, and nonreverting funds to capture unspent funding balances.",
    "This correspondence explained that due to need that does not meet supply subsidy waitlists will be reinstated throughout the state.",
    "This bill was to provide subsidies to childcare employees regardless of income status, in hopes of incentivizing working in childcare. It passed, but then died in the house."
  ),
  link = c(
    "timeline/2018-06-MR-Survey.qmd.pdf",
    "timeline/2020-10-01-RD367.pdf",
    "timeline/2020-11-24-CCDF-transition-plan.pdf",
    "timeline/2021-02-22-Covid-Relief.pdf",
    "timeline/2021-01-28-HB2206.pdf",
    "timeline/2021-HB1800.pdf",
    "timeline/2022-HB30.pdf",
    "timeline/2022-12-6-ProvLet-Copays.pdf",
    "timeline/2022-09-29-Corresp...ider-payment-rates.pdf",
    "timeline/2023-09-20-paidabsenceincrease.pdf",
    "timeline/2024-02-07-HB419.pdf",
    "timeline/2024-06-04-waitlistvendorsletter.pdf",
    "timeline/2024-01-10-subsidysexpandtoCCworkers.pdf"
  ),
  stringsAsFactors = FALSE
)
policy_data$start <- as.character(policy_data$start)






# UI ----------------------------------------------------------------------

ui <- navbarPage(
  title = "Childcare Analysis Project",
  
  header = tagList(
    tags$head(
      tags$style(HTML("
        body {
          background-color: #ffffff;
          font-family: 'Times New Roman', serif;
        }
        .navbar {
          background-color: #2e708c;
        }
        .navbar-default .navbar-nav > li > a {
          color: white;
        }
        .navbar-default .navbar-brand {
          color: white;
          font-family: 'Times New Roman', serif; 
          font-weight: bold;
          font-size: 20px;
        }
        .tab-content {
          background-color: #d3ebf5;
        }
        h2 {
          color: #333333;
          font-family: 'Times New Roman', serif;
          style = font-size:38px; font-weight:bold; margin-bottom:10px;
        }
        p {
          color: #333333;
          font-family: 'Times New Roman', serif;
          style =font-size:20px
        }
      "))
    )
  ),
  
  tabPanel("Background",
           fluidPage(
             div(
               style = "text-align: center;",
               h2("Exploring the Impact of Childcare on Labor Force Outcomes", 
                  style = "font-size:38px; font-weight:bold; margin-bottom:10px;"),
               h3("Data Science for the Public Good", 
                  style = "font-size:17px; font-weight:bold; margin-top:0px; margin-bottom:5px;"),
               h3("Virginia Tech 2025", 
                  style = "font-size:17px; font-weight:bold; margin-top:0px;")
             )
             
             ,
             fluidRow(
               column(
                 width = 6,
                 h4("Background:",
                    style = "font-weight:bold"),
                 p("Childcare acts as a foundational pillar for economic stability and workforce engagement; however, the rising cost of care has become a major barrier to employment. According to the Center for American Progress, the lack of affordable childcare is one of the most significant obstacles to sustained employment, leading to parents reducing hours, turning down opportunities, or exiting the labor force altogether. The burden associated with childcare and the workforce falls disproportionately on mothers, who are more likely to bear caregiving responsibilities. When women are unable to find proper childcare, they are less likely to be employed, reducing household income with potentially long-term career setbacks."),
                 p("Moreover, the affordability and availability of childcare vary widely across different regions nationally and within the state of Virginia. Families living in rural areas or economically disadvantaged regions face additional barriers, including fewer childcare providers. These disparities exacerbate labor market inequalities and deepen the economic precarity of working families."),
                 p("Childcare subsidies help low and moderate-income families afford the cost of childcare, enabling parents to work, look for work, attend school, or participate in vocational training. The primary federal program providing this funding is the Child Care and Development Fund (CCDF), which in the state of Virginia has been administered and maintained by the Virginia Department of Education since 2020, priorly being managed by the Virginia Department of Social Services.")
               ),
               column(
                 width = 6,
                 h4(     ),
                 p("Since 2018, there have been many changes made to the eligibility of subsidies, particularly during and after the COVID-19 pandemic, both nationally and at the state level. In response to the crisis, $9.6 billion in federal funding was provided through the Coronavirus Response and Relief Supplemental Appropriations Act. Enabling the state of Virginia to temporarily expand eligibility and serve more families.The family income threshold was raised to 85% of the federal poverty line, and the eligibility requirements were expanded to parents actively searching for work to qualify for subsidies; an important change that supported families facing job loss or economic instability associated with the pandemic.During this time, copayments and waitlists for subsidies were eliminated. These two supportive measures have been rolled back in 2023 and 2024, respectively, due to budget constraints; however, the pandemic-era guideline for income requirements and job-seeking parents has remained as eligibility requirements for childcare assistance."),
                 h4("Research Questions:",
                    style = "font-weight:bold"),
                 tags$ul(
                   tags$li("How do childcare prices differ across the state of Virginia, specifically between rural and urban areas?"),
                   tags$li("What is the relationship between labor outcomes and the usage of childcare, and how do subsidy usage, gender, and locality play a role in said relationship?"),
                   tags$li("How do changes in Virginia state policies to the Child Care Development Fund (CCDF) impact program participants?")
                 )
               )
             ),
             br(),
             tags$div(
               style = "font-size:12px; text-align:left; margin-top:20px;",
               strong("Sources:"),
               tags$ul(
                 tags$li(tags$a(href = "https://pmc.ncbi.nlm.nih.gov/articles/PMC10575474/",
                                "Access to Early Care and Education in Rural Communities: Implications for Children’s School Readiness", target = "_blank")),
                 tags$li(tags$a(href = "https://vachildcare.com/why-is-early-childhood-education-important/your-child-care-search/financial-assistance/",
                                "Financial Assistance - Child Care Aware of Virginia", target = "_blank")),
                 tags$li(tags$a(href = "https://www.childcare.virginia.gov/families/paying-for-child-care",
                                "Paying for Child Care | Child Care VA", target = "_blank")),
                 tags$li(tags$a(href = "https://www.americanprogress.org/article/child-care-crisis-keeping-women-workforce/#:~:text=According%20to%20the%20poll%2C%20slightly,important%20benefits%20and%20workplace%20supports",
                                "The Child Care Crisis Is Keeping Women Out of the Workforce - Center for American Progress", target = "_blank")),
                 tags$li(tags$a(href = "https://bipartisanpolicy.org/blog/understanding-childcare-challenges-in-rural-america/https://www.ntca.org/sites/default/files/documents/2024-10/understanding-childcare-challenges-in-rural-america.pdf",
                                "Understanding Childcare Challenges in Rural America", target = "_blank")),
                
               ))
           )
  ),
  
  tabPanel("Understanding Policy",
           fluidPage(
             titlePanel("Virginia Childcare Policy Timeline"),
             p("Childcare is drastically shaped by the policies that surround it, especially when it comes to policies regarding childcare subsidies. Below is an interactive timeline that displays major policy changes from 2018 to now that impact childcare subsidies. To see the primary source click on the hyperlink in the document description."),
             fluidRow(
               column(8, timevisOutput("timeline")),
               column(4, uiOutput("policyDetails"))
             ),
             p("Below is a line graph depicting total count of children on childcare subsidies. Click a policy event on the timeline above to see where it falls, highlighted by a yellow line, on the chart."),
             br(),
             fluidRow(
               column(12, echarts4rOutput("line_plot"))
             )
           )
  ),
  
  tabPanel("Maps",
           fluidPage(
             h2("Maps"),
             h2("Virginia Childcare Costs as a Percent of Annual Family Income"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("STUDYYEAR", "Select Year", choices = 2009:2022),
                 selectInput("percentile", "Select Pricing Point:", choices = c("50th Percentile"="M", "75th Percentile"="_75")),
                 selectInput("age_group", "Select Age Group:", choices = c(
                   "0 to 5 Months" = "Bto5",
                   "6 to 11 Months" = "6to11",
                   "12 to 17 Months" = "12to17",
                   "18 to 23 Months" = "18to23",
                   "24 to 29 Months" = "24to29",
                   "30 to 35 Months" = "30to35",
                   "36 to 41 Months" = "36to41",
                   "42 to 47 Months" = "42to47",
                   "48 to 53 Months" = "48to53",
                   "54 Months to School Age" = "54toSA",
                   "School Age" = "SA"
                 )),
                 selectInput("provider", "Select Provider Type:", choices = c("Center Care"="C", "Home Care"="FCC"))
               ),
               mainPanel(
                 leafletOutput("childcareMap", height = "600px")
               )
             )
           )
  ),
  

# Joshua Stuff ------------------------------------------------------------

navbarMenu("Modeling",
           tabPanel("Variables Important to Female Employment",
                    fluidPage(
                      h2("Variables Important to Female Employment"),
                      sidebarLayout(
                        sidebarPanel(
                          h4("Methodology"),
                          p("Describe your regression model, data sources, and sample size."),
                          tags$ul(
                            tags$li("Survey data from CPS and SIPP"),
                            tags$li("DiD model with robust standard errors"),
                            tags$li("Controls: age, education, number of children")
                          )
                        ),
                        mainPanel(
                          h4("Regression Output"),
                          br(),
                        )
                      )
                    )
           ),
           tabPanel("Predicting Employment Outcomes",
                    fluidPage(
                      h2("Predicting Employment Outcomes"),
                      sidebarLayout(
                        sidebarPanel(
                          div(
                            style = "position: sticky; top: 80px;",
                            h4(tags$strong("Methodology")),
                            p("Using Ordinary Least Squares, we analyzed the Survey of Income and Program Participation"),
                            tags$ul(
                              tags$li("National Survey data from 2020 - 2023"),
                              tags$li("Filtered for households with at least one child under the age of 5"),
                              tags$li("Welfare or SS variable was used as a proxy for subsidy")
                            ),
                            p("We find positive impacts of using childcare, espeically in rural areas, however we find a negative impact of 
                              Welfare or Social Security. This may be because of the imprecise nature of our variable, biasing our coeficcients toward 0, as well as
                              a nonrepresentative sample. This motivates us to further explore the impact of subsidy through a Differences-in-Differences Model"),
                            
                            tags$figure(
                              img(
                                src = "images/chart1.png",
                                width = "100%",
                                style = "border:1px solid #ccc; border-radius:8px;",
                                alt = "Key Predictors of Employment Chart"
                              ),
                              tags$figcaption(
                                style = "font-size: 0.9em; color: #555; text-align: center; margin-top: 5px;",
                                "Figure: Coefficient Chart" 
                              )
                            )
                          )
                        ),
                        mainPanel(
                          h4(tags$strong("Model Equation (OLS with Controls)")),
                          withMathJax(
                            helpText("$$
\\begin{aligned}
\\text{Employment}_{i,t} &= \\beta_0 + \\beta_1 \\text{Female}_{i,t} + \\beta_2 \\text{Childcare}_i + \\beta_3 \\text{NonMetro}_{i,t} \\\\
&\\quad + \\beta_4 \\text{WelfareorSS}_{i,t} + \\beta_5 \\text{(NonMetro * Childcare)}_{i,t} \\\\
&\\quad + \\text{Controls}_{i,t} \\boldsymbol{\\gamma} + \\epsilon_{i,t}
\\end{aligned}
$$")
                          ),
                          br(),
                          h4(tags$strong("Key Variables (OLS with Controls)")),
                          htmlOutput("SIPPKey"),
                          tags$div(
                            style = "margin-top: -15px; ; font-size: 0.9em; font-style: italic; font-family: 'Times New Roman', Times, serif;",
                            HTML("Note: <b>+</b> p &lt; 0.1;&nbsp; <b>*</b> p &lt; 0.05;&nbsp; <b>**</b> p &lt; 0.01;&nbsp; <b>***</b> p &lt; 0.001")
                          ),
                          br(),
                          h4(tags$strong("Controls (OLS with Controls)")),
                          htmlOutput("SIPPControls"),
                          tags$div(
                            style = "margin-top: -15px; ; font-size: 0.9em; font-style: italic; font-family: 'Times New Roman', Times, serif;",
                            HTML("Note: <b>+</b> p &lt; 0.1;&nbsp; <b>*</b> p &lt; 0.05;&nbsp; <b>**</b> p &lt; 0.01;&nbsp; <b>***</b> p &lt; 0.001")
                          )
                          
                        )
                      )
                    )
           ),
           tabPanel("Machine Learning Model",
                    fluidPage(
                      h2("Machine Learning Model"),
                      sidebarLayout(
                        sidebarPanel(
                          h4(tags$strong("Methodology")),
                          p("Income and employment status are endogenous, as 
                            individuals may make choices, such as working additional 
                            hours or declining a promotion, 
                            that affect their subsidy eligibility. 
                            To address this endogeneity, we developed five 
                            different machine learning models to predict the income 
                            group each individual would likely belong to. Among these models, 
                            we selected the Random Forest model for our analysis, as it achieved the
                            highest accuracy and kappa score. 
                            Kappa measures the agreement between predicted and actual classifications,
                            adjusted for chance. A kappa score above 0.60 is generally considered good, with values above 0.80 indicating strong agreement."),
                          tags$ul(
                            tags$li("Classifcation and Regression Tree (CART): A decision tree algorithm that splits data into branches to predict outcome."),
                            tags$li("Linear Discriminant Analysis (LDA): A linear classifier that separates classes by maximizing variance between them"),
                            tags$li("Support Vector Machine (SVM): Identifies the optimal boundary (hyperplane) that best separates classes"),
                            tags$li("k-Nearest Neighbors (kNN): Classifies based on the majority vote among the k closest data points"),
                            tags$li("Random Forest (RF): An ensemble of decision trees that boosts accuracy by averaging many trees.")
                          )
                        ),
                        mainPanel(
                          tags$figure(
                            img(
                              src = "images/MachineLearningModel.png",
                              width = "100%",
                              style = "border:1px solid #ccc; border-radius:8px;",
                              alt = "Key Predictors of Employment Chart"
                            ),
                            tags$figcaption(
                              style = "font-size: 0.9em; color: #555; text-align: center; margin-top: 5px;",
                              "Figure: Machine Learning Model Results" 
                            )
                          ),
                          br(),
                          
                        )
                      )
                    )
           ),
           tabPanel("Fixed Effects Differences-in-Differences Model",
                    fluidPage(
                      h2("Fixed Effects Differences-in-Differences Model"),
                      sidebarLayout(
                        sidebarPanel(
                          h4(tags$strong("Methodology")),
                          p("To isolate the impact of subsidy usage on employment, 
                            we implemented a Difference-in-Differences model leveraging Virginia’s 
                            July 2022 expansion of Child Care Development Fund eligibility, from 65% to 85% of the State Median Income.
                            Using data from the Current Population Survey spanning 2019 to 2025, we focused on Virginia households with 
                            children under the age of five. We also incorporated household, month, and year fixed effects to control for
                            time-invariant and temporal factors."),
                        ),
                        mainPanel(
                          h4(tags$strong("Model Equation")),
                          withMathJax(
                            helpText("$$
\\begin{aligned}
\\text{Employment}_{i,t} &= \\beta_0 + \\beta_1 \\, \\text{Predicted_Subsidy}_{i,t} + \\beta_2 \\, \\text{Post}_{i,t} \\\\
&\\quad + \\beta_3 \\, (\\text{Predicted_Subsidy} \\times \\text{Post})_{i,t} + \\text{Controls}_{i,t} \\boldsymbol{\\gamma} \\\\
&\\quad + \\alpha_{\\text{household(FE)}(i)} + \\delta_{\\text{month(FE)}(t)} + \\theta_{\\text{year(FE)}(t)} + \\epsilon_{i,t}
\\end{aligned}
$$")
                          ),
                            br(),
                            h4(tags$strong("Key Variables")),
                            htmlOutput("CPSKey"),
                            tags$div(
                              style = "margin-top: -15px; ; font-size: 0.9em; font-style: italic; font-family: 'Times New Roman', Times, serif;",
                              HTML("Note: <b>+</b> p &lt; 0.1;&nbsp; <b>*</b> p &lt; 0.05;&nbsp; <b>**</b> p &lt; 0.01;&nbsp; <b>***</b> p &lt; 0.001")
                            ),
                            br(),
                            h4(tags$strong("Controls")),
                            htmlOutput("CPSControls"),
                            tags$div(
                              style = "margin-top: -15px; ; font-size: 0.9em; font-style: italic; font-family: 'Times New Roman', Times, serif;",
                              HTML("Note: <b>+</b> p &lt; 0.1;&nbsp; <b>*</b> p &lt; 0.05;&nbsp; <b>**</b> p &lt; 0.01;&nbsp; <b>***</b> p &lt; 0.001")
                            )
                            
                          )
                        )
                      )
                    )),

  #ABOUT US-------
  tabPanel("About Us",
           fluidPage(
             div(
               style = "display: flex; justify-content: center;",
               div(
                 style = "text-align: center; max-width: 900px;",
                 
                 # --- Header ---
                 h2("About Us", style = "font-size:38px; font-weight:bold; margin-bottom:10px;"),
                 
                 p(HTML('Data Science for the Public Good (DSPG) Young Scholars program is an intensive summer program hosted by the Virginia Tech Department of Agricultural and Applied Economics and the Virginia Cooperative Extension Service. This summer marks five years of the immersive ten-week program enabling students from a variety of colleges and universities to have hands-on educational experience working with data analysis, statistics, R coding, and the social sciences. These skills are applied to real-world social issues via team-based research projects to enable students to utilize data for real-world issues such as policy, industry, academic research, and community engagement. For more information on the program, other teams\' research, and information on how to apply, visit the official <a href="https://aaec.vt.edu/academics/undergraduate/dspg.html" target="_blank">VT DSPG site</a>.')),
                 
                 # --- Undergraduate Fellows ---
                 h3("Undergraduate Fellows", style ="font-size:32px; font-weight:bold; margin-top:0px"),
                 fluidRow(
                   column(
                     width = 4,
                     div(style = "text-align:center;",
                         img(src = "images/grace.jpg", height = "200px", style = "border-radius: 50%; margin-bottom: 10px;"),
                         br(),
                         tags$a("Grace Mullins", href = "https://www.linkedin.com/in/grace-eliz-mullins/", target = "_blank",
                                style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"),
                         p("Berea College '27", style = "font-size:14px; color:#333333; margin-top:0px; margin-bottom:0px;"), 
                         p("Sociology", style = "font-size:14px; color:#333333; margin-top:0px;") )
                   ),
                   column(
                     width = 4,
                     div(style = "text-align:center;",
                         img(src = "images/yin.jpeg", height = "200px", style = "border-radius: 50%; margin-bottom: 10px;"),
                         br(),
                         tags$a("Yin Kyay Wai", href = "https://www.linkedin.com/in/yin-kyay-wai/", target = "_blank",
                                style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"),
                         p("Berea College '26", style = "font-size:14px; color:#333333; margin-top:0px; margin-bottom:0px;"),
                         p("Computer Science", style = "font-size:14px; color:#333333; margin-top:0px;") )
                   ),
                   column(
                     width = 4,
                     div(style = "text-align:center;",
                         img(src = "images/joshua.jpeg", height = "200px", style = "border-radius: 50%; margin-bottom: 10px;"),
                         br(),
                         tags$a("Joshua Yoo", href = "https://www.linkedin.com/in/joshua-yoo-6b389524a/", target = "_blank",
                                style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"),
                         p("The College of William and Mary '26",style = "font-size:14px; color:#333333; margin-top:0px; margin-bottom:0px;"),
                         p("Finance and Computational and Applied Mathematics", style = "font-size:14px; color:#333333; margin-top:0px;")
                     )
                   )
                 ),
                 
                 # --- Graduate Fellow Centered ---
                 h3("Graduate Fellow", style ="font-size:32px; font-weight:bold; margin-top:30px"),
                 fluidRow(
                   column(width = 4), # empty for centering
                   column(
                     width = 4,
                     div(style = "text-align:center;",
                         img(src = "images/xiaoyi.jpeg", height = "200px", style = "border-radius: 50%; margin-bottom: 10px;"),
                         br(),
                         tags$a("Xiaoyi Zhao", href = "https://www.linkedin.com/in/xiaoyi-zhao-32457a248/", target = "_blank",
                                style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"),
                         p("Virginia Tech", style = "font-size:14px; color:#333333; margin-top:0px; margin-bottom:0px;"), 
                         p("Applied Agriculture and Economics", style = "font-size:14px; color:#333333; margin-top:0px;")
                     )
                   ),
                   column(width = 4) # empty for centering
                 ),
                 
                 # --- Faculty ---
                 h3("Faculty", style="font-size:32px; font-weight:bold; margin-top:30px"),
                 fluidRow(
                   column(
                     width = 4,
                     div(style = "text-align:center;",
                         img(src = "images/wang.png", height = "200px",
                             style = "border-radius: 50%; margin-bottom: 10px;"),
                         br(),
                         tags$a("Dr.Le Wang", href = "https://www.linkedin.com/in/le-wang-872aa529/", target = "_blank",
                                style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"),
                         p("Virginia Tech", style = "font-size:14px; color:#333333; margin-top:0px; margin-bottom:0px;"),
                         p("David M. Kohl Chair & Professor; Director, Kohl Centre and DSPG Program", 
                           style = "font-size:14px; color:#333333; margin-top:0px; margin-bottom:0px;")
                     )
                   ),
                   column(
                     width = 4,
                     div(style = "text-align:center;",
                         img(src = "images/bradburn.jpg", height = "200px",
                             style = "border-radius: 50%; margin-bottom: 10px;"),
                         br(),
                         tags$a("Dr.Isabel Bradburn", href = "https://www.linkedin.com/in/isabel-bradburn-3418a964/", target = "_blank",
                                style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"),
                         p("Virginia Tech", style = "font-size:14px; color:#333333; margin-top:0px; margin-bottom:0px;"),
                         p("Research Director, Child Development Center for Learning & Research", 
                           style = "font-size:14px; color:#333333; margin-top:0px; margin-bottom:0px;")
                     )
                   ),
                   column(
                     width = 4,
                     div(style = "text-align:center;",
                         img(src = "images/Chunbe.png", height = "200px",
                             style = "border-radius: 50%; margin-bottom: 10px;"),
                         br(),
                         tags$a("Dr.Chunbei Wang", href = "https://www.linkedin.com/in/chunbei-wang-2242a0126/", target = "_blank",
                                style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"),
                         p("Virginia Tech", style = "font-size:14px; color:#333333; margin-top:0px; margin-bottom:0px;"),
                         p("Associate Professor & Graduate Program Director", 
                           style = "font-size:14px; color:#333333; margin-top:0px; margin-bottom:0px;")
                     )
                   )
                 )
               )
             )
           )
  ),

  tabPanel("Acknowledgements",
           fluidPage(
             div(
               style = "max-width: 1000px; margin: auto;",  # wrapper for everything
               
               # --- Centered Title ---
               h2("Data", style = "text-align:center; margin-bottom:20px;"),
               
               # --- Flex container for all data blocks ---
               div(
                 style = "display: flex; flex-direction: column; gap: 30px; text-align:left;",
            
               
               # --- Flex container for all data blocks ---
               div(
                 style = "display: flex; flex-direction: column; gap: 30px;",
                 
                 # --- CPS ---
                 div(
                   style = "display: flex; align-items: flex-start; gap: 20px;",
                   img(src = "images/CPS.avif", height = "120px", style = "flex-shrink:0;"),
                   div(
                     tags$a(
                       "Current Population Survey",
                       href = "https://www.census.gov/programs-surveys/cps.html",
                       target = "_blank",
                       style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"
                     ),
                     p('"The Current Population Survey (CPS) is sponsored by both the U.S. Census Bureau and the U.S. Bureau of Labor Statistics (BLS), and is the primary source of labor force statistics for the population of the USA."- Taken from the CPS website.',
                       style = "font-size:14px; color:#333333; margin-top:5px;")
                   )
                 ),
                 
                 # --- NDCP ---
                 div(
                   style = "display: flex; align-items: flex-start; gap: 20px;",
                   img(src = "images/ndcp.png", height = "120px", style = "flex-shrink:0;"),
                   div(
                     tags$a(
                       "National Database of Childcare Prices",
                       href = "https://www.dol.gov/agencies/wb/topics/featured-childcare",
                       target = "_blank",
                       style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"
                     ),
                     p('"The National Database of Childcare Prices (NDCP) is the most comprehensive federal source of childcare prices at the county level. The database offers childcare price data by childcare provider type, age of children, and county characteristics. Data are available from 2008 through 2022."-Taken from NCDP',
                       style = "font-size:14px; color:#333333; margin-top:5px;")
                   )
                 ),
                 
                 # --- SIPP ---
                 div(
                   style = "display: flex; align-items: flex-start; gap: 20px;",
                   img(src = "images/SIPP.png", height = "120px", style = "flex-shrink:0;"),
                   div(
                     tags$a(
                       "Survey of Income and Program Participation",
                       href = "https://www.census.gov/programs-surveys/sipp.html",
                       target = "_blank",
                       style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"
                     ),
                     p('"SIPP is a nationally representative longitudinal survey that provides comprehensive information on the dynamics of income, employment, household composition, and government program participation. SIPP is also a leading source of data on economic well-being, family dynamics, education, wealth, health insurance, child care, and food security. The survey interviews individuals for several years and provides monthly data about changes in household and family composition and economic circumstances over time."-Taken from the SIPP website',
                       style = "font-size:14px; color:#333333; margin-top:5px;")
                   )
                 ),
                 
                 # --- VDOE ---
                 div(
                   style = "display: flex; align-items: flex-start; gap: 20px;",
                   img(src = "images/VDOE.jpg", height = "120px", style = "flex-shrink:0;"),
                   div(
                     tags$a("Virginia Department of Education",
                            style = "font-size:16px; font-weight:bold; text-decoration:underline; color:#0072B2;"),
                     p("Special thanks to the Virginia Department of Education (VDOE) for consulting with our team and providing data on expenditures and total count of children on subsidies by county in the state of Virginia.",
                       style = "font-size:14px; color:#333333; margin-top:5px;")
                   )
                 )
               ),
               
               # --- Final text at the bottom ---
               div(
                 style = "margin-top:30px; text-align:center;",
                 p(
                   tags$b(
                     "This work is supported by the U.S. Department of Agriculture, National Institute of Food and Agriculture as part of the DATA-ACRE program [grant no. 2022-67037-36639 / project accession no. 2021-10424]."
                   ),
                   style = "font-size:14px; color:#333333;"
                 )
               )
             )
           )
  )
))


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    cost_col <- paste0(input$percentile, input$provider, input$age_group)
    df <- map_data_all %>%
      filter(STUDYYEAR == input$STUDYYEAR) %>%
      mutate(cost_pct_income = (.data[[cost_col]] * 52) / MFI) %>%
      filter(!is.na(cost_pct_income), is.finite(cost_pct_income), cost_pct_income > 0)
    df
  })
  
  output$childcareMap <- renderLeaflet({
    df <- filtered_data()
    pal <- colorNumeric("viridis", df$cost_pct_income, na.color = "gray90")
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(cost_pct_income),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        label = ~paste0(NAME, ": ", scales::percent(cost_pct_income, accuracy = 1)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = df$cost_pct_income,
        title = "Cost as % of Income",
        labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x)
      )
  })
  
  # Timeline outputs
  output$timeline <- renderTimevis({
    timevis(
      data = policy_data,
      options = list(selectable = TRUE)
    )
  })
  
  output$policyDetails <- renderUI({
    selected_id <- input$timeline_selected
    
    if (!is.null(selected_id)) {
      selected_row <- policy_data[policy_data$id == selected_id, ]
      
      tagList(
        h4(selected_row$content),
        p(selected_row$summary),
        tags$a(
          href = selected_row$link,
          target = "_blank",
          "📄 View Document",
          style = "color: blue; text-decoration: underline; cursor: pointer;"
        )
      )
    } else {
      h5("📌 Click a timeline event to view its summary and document.")
    }
  })
  selected_range <- reactiveVal(NULL)
  
  observeEvent(input$timeline_selected, {
    selected_id <- input$timeline_selected
    if (!is.null(selected_id)) {
      start_date <- as.Date(policy_data$start[policy_data$id == selected_id])
      end_date <- policy_data$end[policy_data$id == selected_id]
      
      if (!is.na(end_date)) {
        end_date <- as.Date(end_date)
        selected_range(c(start_date, end_date))
      } else {
        # fallback: +/- 30 days if no end date
        selected_range(c(start_date - 30, start_date + 30))
      }
    } else {
      selected_range(NULL)
    }
  })
  
  selected_range <- reactiveVal(NULL)
  
  observeEvent(input$timeline_selected, {
    selected_id <- input$timeline_selected
    if (!is.null(selected_id)) {
      start_date <- as.Date(policy_data$start[policy_data$id == selected_id])
      end_date <- policy_data$end[policy_data$id == selected_id]
      
      if (!is.na(end_date)) {
        end_date <- as.Date(end_date)
        selected_range(c(start_date, end_date))
      } else {
        # fallback: +/- 30 days if no end date
        selected_range(c(start_date - 30, start_date + 30))
      }
    } else {
      selected_range(NULL)
    }
  })
  
  # TIMELINE AND GRAPH BELOW
  output$line_plot <- renderEcharts4r({
    rural_urban_by_date <- VDOE_data %>%
      group_by(date, Urban.y) %>%
      summarise(Total_Count = sum(Total_Count, na.rm = TRUE), .groups = "drop") %>%
      mutate(Urban.y = ifelse(Urban.y == 1, "Urban", "Rural"))
    
    p <- rural_urban_by_date %>%
      group_by(Urban.y) %>%
      e_charts(date) %>%
      e_line(Total_Count, name = NULL) %>%
      e_tooltip(trigger = "axis") %>%
      e_legend(right = 10) %>%
      e_x_axis(
        type = "time",
        name = "Date",
        axisLabel = list(
          formatter = htmlwidgets::JS("function (value) { return new Date(value).toLocaleDateString(); }")
        )
      ) %>%
      e_y_axis(name = "Total Count", min = 0) %>%
      e_title("Total Count of Children on Subsidies Over Time (Rural vs Urban)") %>%
      e_datazoom(type = "slider", start = 0, end = 100)
    
    for (d in as.Date(policy_data$start)) {
      p <- p %>%
        e_mark_line(
          data = data.frame(xAxis = as.character(d)),
          symbol = "none",
          lineStyle = list(type = "dashed", color = "gray")
        )
    }
    
    if (!is.null(selected_range())) {
      range <- selected_range()
      p <- p %>%
        e_mark_area(
          data = list(
            list(xAxis = as.character(range[1])),
            list(xAxis = as.character(range[2]))
          ),
          itemStyle = list(color = "rgba(255, 165, 0, 0.3)")
        ) %>%
        e_mark_line(
          data = data.frame(xAxis = as.character(range[1])),
          symbol = "none",
          lineStyle = list(type = "solid", color = "blue", width = 2)
        ) %>%
        e_mark_line(
          data = data.frame(xAxis = as.character(range[2])),
          symbol = "none",
          lineStyle = list(type = "solid", color = "blue", width = 2)
        )
    }
    
    p
  })
        
    
   

# Joshua Tables -----------------------------------------------------------
  
    output$SIPPKey <- renderUI({
      SIPPKey <- data.frame(
        Variables = c("Intercept","Female", "ChildCare", "NonMetro" , "WelfareorSS", "NonMetro x ChildCare"),
        Coefficients = c(0.898,-0.117,0.096, -0.021, -.030, 0.121),
        `P-Value` = c("2e-16***","1.43e-11***","5.80e-08***", "2e-16***","0.151","0.011*"),
        Interpretation = c(
          "The baseline employment likliehood in the dataset was 89.81%",
          "Women have a 11.7% lower likliehood of employment compared to men",
          "Usage of Childcare increased employment likliehood by 9.6%",
          "Non-Metro status was associated with a 2.1% decrease in employment",
          "Usage of Welfare or Social Security lead to a 3% decrease in employment outcomes (Not significnat)",
          "Childcare usage in Non-Metro areas was associated with a significant 12.1% increase in employment"
        )
      )
      
      html_table <- SIPPKey %>%
        kable(
          format = "html",
          align = "lcc",
          col.names = c("Variables", "Coefficients", "P-Value", "Interpretation"),
          table.attr = 'style="border: 2px solid black; border-collapse: separate; border-spacing: 0;"'
        ) %>%
        kable_styling(
          bootstrap_options = c("striped", "hover", "condensed", "responsive"),
          full_width = FALSE,
          position = "center"
        ) %>%
        column_spec(3, width = "120px")  
      HTML(html_table)
      
    })
  
  output$SIPPControls <- renderUI({
    SIPPControls <- data.frame(
      Variables = c(
        "Single Family",
        "Education: Graduate or Professional Degree",
        "Education: High School or Some College",
        "Education: No High School Diploma",
        "Parent Age",
        "Race: Asian",
        "Race: Black",
        "Race: Residual",
        "Kids Under 5: 2",
        "Kids Under 5: 3",
        "Kids Under 5: 4"
      ),
      Coefficients = c(0.069,
                       0.032,-0.108,-0.262,
                       0.002,
                       -0.138, 0.126,-0.011,
                       -0.066,-0.159,-0.140),
      `P-Value` = c(
        "8.23e-13***",
        ".085+", "2e-16***", "2e-16***",
        ".006**",
        "1.47e-09***","2e-16***","0.559",
        "3.29e-11***", "7.31e-12***","0.006**"
      ),
      Interpretation = c(
        "Single Parents have a 6.9% higher likliehood of employment",
        "Graduate or Professional degree holders have a 3.2% higher likiehood of employment compared to a bachelors",
        "High School degree holders have a 10.8% lower chance of employment compared to a bachelors",
        "No HS degree holders have a 26.2% lower likliehood of employment compared to a bachelors",
        "Each additional year in parental age is associated with a 0.2% increase in employment likelihood",
        "Asians have a 13.8% lower chance of employment compared to whites",
        "Blacks have a 12.6% higher chance of employment compared to whites",
        "Individual in the residual race group have a 9.2% higher chance of employment compared to whites",
        "Parents with 2 kids under 5 have a 5.9% lower chance of employment",
        "Parents with 3 kids under 5 have a 14.4% lower chance of employment",
        "Parents with 4 kids under 5 have a 10.5% higher chance of employment"
      )
    )
    
    html_table <- SIPPControls %>%
      kable(
        format = "html",
        align = "lcc",
        col.names = c("Variables", "Coefficients", "P-Value", "Interpretation"),
        table.attr = 'style="border: 2px solid black; border-collapse: separate; border-spacing: 0;"'
      ) %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width = FALSE,
        position = "center"
      ) %>%
      column_spec(3, width = "120px")  
    HTML(html_table)
    
  })
  
  
  output$CPSKey <- renderUI({
    CPSKey <- data.frame(
        Variables = c(
          "predicted_subsidy: Newly_Eligible", 
          "Post", 
          "Newly Eligible × Post"
        ),
        Coefficients = c(
         0.054, 0.007, 0.099
        ),
        P_Value = c(
          "0.170", "0.875", "0.0468*"
        ),
        Interpretation = c(
          "Newly-Eligible households were 5.4% more likely to be employed (pre-period)",
          "Post-period dummy had no significant effect by itself",
          "Newly-Eligible households saw a 9.9% employment increase post-policy"
        )
        )
    
    html_table <- CPSKey %>%
      kable(
        format = "html",
        align = "lcc",
        col.names = c("Variables", "Coefficients", "P-Value", "Interpretation"),
        table.attr = 'style="border: 2px solid black; border-collapse: separate; border-spacing: 0;"'
      ) %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width = FALSE,
        position = "center"
      ) %>%
      column_spec(3, width = "120px")  
    HTML(html_table)
    
  })
  
  output$CPSControls <- renderUI({
    CPSControls <- data.frame(
        Variables = c(
          "predicted_subsidy: Never_Eligible", 
          "AGE", 
          "NCHLT5", 
          "Married", 
          "Female", 
          "Race: American Indian", 
          "Race: Asian or Pacific Islander", 
          "Race: Black", 
          "Race: White", 
          "Rural", 
          "Education: Graduate/Prof Degree", 
          "Education: HS", 
          "Education: No HS", 
          "Education: Some College", 
          "Never Eligible × Post"
        ),
        Coefficients = c(
          0.118, 0.005, -0.063,
          0.003, -0.188, -0.010, -0.122, -0.094,
          -0.063, -0.045, 0.035, -0.193, -0.224,
          -0.193, 0.029
        ),
        P_Value = c(
          "0.026*", "0.101", "0.0002***",
          "0.944", "<2e-16***", "0.980", "0.173", "0.318",
          "0.356", "2.53e-08***", "0.237", "5.57e-05***", "0.0057**",
          "1.04e-06***", "0.509" 
        ),
        Interpretation = c(
          "Never-Eligible households were 11.8% more likely to be employed (pre-period)",
          "Each year of age slightly increased employment probability",
          "Each child under 5 decreased employment by 6.3%",
          "Marriage had no significant impact on employment",
          "Females were 18.8% less likely to be employed than males",
          "No significant effect for American Indian category",
          "Asian/Pacific Islanders showed a non-significant decrease in employment",
          "Black individuals had lower employment, but not significantly",
          "White individuals also had lower employment (not significant)",
          "Living in a rural area decreased employment by 4.5%",
          "Graduate/Professional degrees showed a modest, non-significant boost",
          "High school education reduced employment likelihood significantly",
          "No HS reduced employment likelihood by 22.4%",
          "Some college reduced employment likelihood by 19.3%",
          "No significant interaction effect for Never-Eligible post-period"
        )
    )
    
    html_table <- CPSControls %>%
      kable(
        format = "html",
        align = "lcc",
        col.names = c("Variables", "Coefficients", "P-Value", "Interpretation"),
        table.attr = 'style="border: 2px solid black; border-collapse: separate; border-spacing: 0;"'
      ) %>%
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed", "responsive"),
        full_width = FALSE,
        position = "center"
      ) %>%
      column_spec(3, width = "120px")  
    HTML(html_table)
    
  })
  

    
}
# Run App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)



