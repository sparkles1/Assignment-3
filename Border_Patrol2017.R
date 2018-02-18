#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)

mydata <- read.csv("BP Apprehensions 2017.csv")

# Define UI for application that draws a barplot
ui <- fluidPage(
  
  # Application title
  titlePanel("Shiny Practice App - Border Patrol Apprehensions in 2017"),
  
  #Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("Sectorname",
                  "Select a Sector",
                  choices = mydata$Sector)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("barplot")
    )
  )
)

# Define server logic required to draw a barplot
server <- function(input, output) {
  
  output$barplot <- renderPlot({
    
    barplot(height = as.matrix(mydata[mydata$Sector == input$Sectorname, 2:13]), 
            main = input$Sectorname,
            ylab = "Number of Apprehensions",
            xlab = "Month",
            col = "Green",
            las = 2)
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

