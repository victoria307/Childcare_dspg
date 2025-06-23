library(shiny)

ui <- navbarPage(
  title = "Childcare Analysis Project",
  
  
  header = tagList(
    tags$head(
      tags$style(HTML("
        body {
          background-color: #ffffff;
          font-family: 'Times New Roman', serif; /* Ensure body uses Times New Roman */
        }
        .navbar {
          background-color: #800000;
        }
        .navbar-default .navbar-nav > li > a {
          color: white;
        }
        .navbar-default .navbar-brand { /* Apply Times New Roman, bold, and larger font to the title */
          color: white;
          font-family: 'Times New Roman', serif; 
          font-weight: bold; /* Make the title bold */
          font-size: 20px;   /* Make the title slightly larger */
        }
        .tab-content {
          background-color: #ffffff;
          padding: 20px;
          border-radius: 5px;
        }
        h2 {
          color: #2e8b57;
          font-family: 'Times New Roman', serif; /* Also apply to h2 for consistency */
        }
        p {
          color: #333333;
          font-family: 'Times New Roman', serif; /* Also apply to p for consistency */
        }
      "))
    )
  ),
  
  
  tabPanel("Background",
           fluidPage(
             h2("Background"),
             p("Write your background information here.")
           )
  ),
  
  tabPanel("Methodology",
           fluidPage(
             h2("Methodology"),
             p("Details of your methodology.")
           )
  ),
  
  tabPanel("Timeline",
           fluidPage(
             h2("Timeline"),
             p("Timeline content goes here.")
           )
  ),
  
  tabPanel("Text Mining Info",
           fluidPage(
             h2("Text Mining Info"),
             p("Explain text mining methods or results.")
           )
  ),
  
  tabPanel("Maps",
           fluidPage(
             h2("Maps"),
             p("Insert your map visualizations.")
           )
  ),
  
  tabPanel("Modeling",
           fluidPage(
             h2("Modeling"),
             p("Describe your models or predictions.")
           )
  ),
  
  tabPanel("Thank You",
           fluidPage(
             h2("Thank You"),
             p("Closing remarks and acknowledgments.")
           )
  )
)

server <- function(input, output, session) {
}

shinyApp(ui = ui, server = server)
