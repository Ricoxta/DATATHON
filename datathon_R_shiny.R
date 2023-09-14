#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
newStore <- read_delim("sales_data_2017_2018_for_tableau_with_new_date_columns.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Datathon Store Revamp"),
    tabsetPanel(
      tabPanel(
        "Overview",
        p("This will show the names and data overview for the store."),
        mainPanel(tableOutput("dataOv"))
      ),
      tabPanel(
        "Finding Most Profitable Month for Store",
        p("This will be using a column graph in order to find when the store is the most profitable."),
        mainPanel(plotOutput("profitPlot"))
      ),
      tabPanel(
        "Hot Items",
        p("This will contain stacked columns with the top 3 highest selling products each month
            which the user will be able to change the category of. "),
        sidebarLayout(
          sidebarPanel(
            radioButtons("mainCat", "Choose a main category", choices =
             list("Fresh Produce", "Beverages", "Pantry Staples", "Snacks", "Flowers", "Breads & Bakery", "Bag", "Miscellaneous"),
             selected = "Fresh Produce")
          )
        ),
        mainPanel(tableOutput("usub"))
      ),
      tabPanel(
        "Reducing Losses",
        p("Unknown yet")
      )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$dataOv <- renderTable({
      head(newStore)
    })
    
    
  #Graph for the profits of the store overall(make it to be interactive between before/after change)
  #also when you hover it shows the value at the top of the column
    output$profitPlot <- renderPlot({
       newStore %>% 
        group_by(month_number) %>% 
        filter(year == 2018) %>%
        mutate(hot = sum(total_profit) / 1000000) %>% #1 million
        ggplot(aes(month_name, hot , fill = month_name)) +
        geom_col()+
      scale_x_discrete(limits = month.name)+
        labs(x = "Month", y = "Profits(millions)")
      
      
    })
    
    output$hotItems <- renderPlot({
      newStore %>% 
        filter(year == 2018)  
        
    })
output$usub <- renderTable({
  newStore %>% 
    summarise(unique(main_category))
})

#---------------------------------END-------------------------------------------
}
# Run the application 
shinyApp(ui = ui, server = server)
