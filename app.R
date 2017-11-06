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
library(ggplot2)
library(tidyr)
library(stringr)

# Reading in the data
collegedata <- read_excel("MungingProjectsDataofInterest.xlsx")
collegedata$STABBR <- factor(collegedata$STABBR)
collegedata$Year <- as.Date(as.character(collegedata$Year), format="%Y")

# Columns
independent_variables <- c("Percentage of First-Time College Students", 
                           "Household Income of Dependent Students", 
                           "Income of Independent Students", 
                           "Median Student Debt at Graduation",
                           "Percentage of Students that are Dependents"
                           )

colnames(collegedata)[9:13] <- independent_variables

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Information by State"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectizeInput("states",
                     "State",
                     levels(collegedata$STABBR),
                     selected = "FL", 
                     multiple=FALSE), 
         checkboxGroupInput("variables",
                            "Variables to Plot:",
                            choices = independent_variables,
                            selected = independent_variables[2])
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("linePlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$linePlot <- renderPlot(
    collegedata[collegedata$STABBR %in% input$states,] %>%
      group_by(Year) %>% 
      select(input$variables) %>%
      mutate_all(median) %>%
      gather(variable, value, -Year) %>%
      ggplot() + 
        labs(y=NULL, x="Year", color="Variable") + 
        theme_bw() + 
        scale_color_discrete(labels = function(x){str_wrap(x, width = 20)}) + 
        scale_y_continuous(labels = scales::comma) + 
        scale_x_date(date_breaks = "1 year", minor_breaks=NULL, date_labels="%Y") + 
        geom_line(aes(x=Year, y=value, color=variable))
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

