library(shiny)
library("ggvis")

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Hello Shiny!"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    
    
    selectInput("select_gen", 
                label = "Choose a target",
                choices = c("male","female"),
                selected = "female"),
    selectInput("select_set", 
                label = "Choose a set to display",
                choices = c("set1","set2","set3","set4","set5"),
                selected = "set2"),
    sliderInput("n", "Number of points", min = 1, max = 10,
                value = 10, step = 1)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    ggvisOutput("plot"),
    tabsetPanel(
      tabPanel('Highest ',
               tableOutput("maxmet")),
      tabPanel('Full Table',
               tableOutput("mytable"))
    )
   
    )
))



