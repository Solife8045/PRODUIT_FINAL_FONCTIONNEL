
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(viridis)


load("MY WORKSPACE_RAPPORTPROJET.RData")


## ==========================
## App Shiny – Dashboard EDA
## ==========================


ui <- fluidPage(
  titlePanel("Dashboard ventes & logistique"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Période :",
                     start = min(AmazonSaleReport$Date),
                     end   = max(AmazonSaleReport$Date)),
      selectInput("market_view", "Marketplace catalogue :",
                  choices = c("May_2022_normalise", "PLMarch2021_normalise")),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("CA quotidien Amazon", plotlyOutput("plot_amazon_ts")),
        tabPanel("CA quotidien International", plotlyOutput("plot_intl_ts")),
        tabPanel("Top SKUs Amazon", plotOutput("plot_top_sku")),
        tabPanel("Heatmap CA client (Intl.)", plotOutput("plot_intl_heat")),
        tabPanel("Prix catalogue", plotOutput("plot_catalog_heat")),
        tabPanel("Logistique", plotOutput("plot_cloud_bar"))
      )
    )
  )
)


server <- function(input, output, session){
  
  # Filtrage des périodes
  amazon_daily_reac <- reactive({
    amazon_daily %>% 
      filter(Date >= input$date_range[1],
             Date <= input$date_range[2])
  })
  
  intl_daily_reac <- reactive({
    intl_daily %>% 
      filter(date >= input$date_range[1],
             date <= input$date_range[2])
  })
  
  output$plot_amazon_ts <- renderPlotly({
    gg <- ggplot(amazon_daily_reac(), aes(x = Date, y = ca_total)) +
      geom_line() +
      labs(title = "CA quotidien – Amazon",
           x = "Date", y = "CA quotidien")
    ggplotly(gg)
  })
  
  output$plot_intl_ts <- renderPlotly({
    gg <- ggplot(intl_daily_reac(), aes(x = date, y = ca_total)) +
      geom_line() +
      labs(title = "CA quotidien – International cohérent",
           x = "Date", y = "CA quotidien")
    ggplotly(gg)
  })
  
  output$plot_top_sku <- renderPlot({
    gg_top_sku
  })
  
  output$plot_intl_heat <- renderPlot({
    gg_intl_heat
  })
  
  output$plot_catalog_heat <- renderPlot({
    if(input$market_view == "May_2022_normalise"){
      gg_may_heat
    } else {
      gg_plm_heat
    }
  })
  
  output$plot_cloud_bar <- renderPlot({
    gg_cloud_bar
  })
}


shinyApp(ui, server)