#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(dplyr)

# Reading in the data
data <- read_excel("MungingProjectsDataofInterest.xlsx")
data$STABBR <- factor(data$STABBR)

# Columns
independent_variables <- c("Percentage of fist time college students", 
                           "Houlsehold Income of Dependent Students", 
                           "Income of Independent Students", 
                           "Median Student Debt at Graduation",
                           "Percentage of Students that are Dependents"
                           )

colnames(data)[9:13] <- independent_variables

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Information by State"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectizeInput("state",
                     "State",
                     levels(data$STABBR),
                     selected = "FL"), 
         checkboxGroupInput("metrics",
                            "Options",
                            choices = independent_variables)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = 25)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

