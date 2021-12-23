library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Stock Earnings Tool"),
    textInput("tick", "Please Enter Stock Ticker:", value = "", width = NULL, placeholder = NULL),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of Earnings Releases Back:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "All-Time Earnings Performance", 
              plotOutput("totalPlot"),
            ),
            
            tabPanel("Choose Specific Earning",
                     plotOutput("chooseEarning")
            ),
            
            tabPanel("Earnings Performance Since",
                     plotOutput("allEarning"),
            ),
            tabPanel("Seasonality Analysis",
                     plotOutput("quarterEarning"),
            )
          )
        )
    )
))




